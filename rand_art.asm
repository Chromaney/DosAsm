org 0x100
section .text

start:
    ; set video mode
    mov ax, 0x0013
    int 0x10

    ; init RNG
    xor ax, ax
    int 0x1a ; dx - lower word of system time
    or dx, 0x0050 ; avoid all-zero state
    mov [rng_state], dx

    cld

    ; set all init coords
    ; first call to fill_word_array - no value limiting
    mov cx, param_num
    mov di, var_coords_blobs
    call fill_word_array

    ; set random starting point for pattern (use prev rand since they have different functions)
    mov [pattern_frame], ax

    ; set all velocities
    ; di is put at start of this array
    ; now and later fill_word_array uses limiting with value from dx
    mov cx, param_num
    mov dx, 0x01ff
    call fill_word_array

    ; main loop
    mov cx, -1 ; cx = drawn frames counter

    ; wait for next frame
wait_scr:
    mov dx, 0x03da
    in al, dx
    test al, 8
    jz wait_scr

    push di ; remember pointer to accels

    inc cx
    jnz no_chng_accels
    
    ; change all accels
    ; di is put at start of this array
    ; bx is already correct filling function
    mov cx, param_num
    mov dx, 0x001f
    call fill_word_array

    mov cx, -6 ; reset cx

no_chng_accels:
    ; change pattern counter based on rendered frames
    inc word [pattern_frame]
    
    ; update screen
    push cx
    std
    
    ; update all kinematics (palette & blobs)
    mov cx, param_num
    mov di, (var_coords_blobs + 2 * param_num - 2) ; pointer to vars
    mov si, (var_coords_blobs + 2 * 3 * blob_num - 2 + 6) ; pointer to sz
uk_loop:

    mov dx, [di + (2 * param_num)] ; old veloc
    add dx, [di + (4 * param_num)] ; new veloc (old + accel)

    ; limit veloc in dx
    mov ax, 0x0280
    cmp ax, dx
    jns ukl_vlc_chk2
    mov dx, ax
ukl_vlc_chk2:
    neg ax
    cmp dx, ax
    jns ukl_vlc_chk_end
    mov dx, ax
ukl_vlc_chk_end:

    mov ax, [di] ; read coord
    add ax, dx ; update coord

    cmp cx, (3 * blob_num + 1)
    jns ukl_no_coord_corr

    ; update pointer to sz
    push si
    sub si, 6
    cmp di, si
    pop si
    jnz ukl_proc_coord
    sub si, 6
    jmp ukl_no_coord_corr

ukl_proc_coord:
    ; limit coord
    mov bx, [si]

    ; transform sz in bx
    xchg ax, bx
    call corr_blob_sz
    xchg ax, bx

    neg bx
    add bx, 0x7fff ; (max_x - sz)

ukl_ax_and:
    and ax, 0x7fff ; correction on first iteration when values are init-ed to full word range
    ; ^ changed to 0xffff after first call so does nothing afterwards
    ; ^ only for blob coords
    ; ^ could be omitted but then too many blobs will spawn on 0

    ; SF for ax already in correct state
    jns ukl_coo_gt
    xor ax, ax
    jmp ukl_coo_corrd
ukl_coo_gt:
    cmp ax, bx
    js ukl_coo_lt
    mov ax, bx
ukl_coo_corrd:
    ; reverse veloc if edge was hit
    neg dx
ukl_coo_lt:
    ; coord in range

ukl_no_coord_corr:

    mov [di + (2 * param_num)], dx ; write new veloc
    stosw ; write new coord, dec di by 2
    loop uk_loop

    mov byte [ukl_ax_and + 2], 0xff
    ; update all kinematics end

    ; update palette
    mov cx, 3
    mov si, (var_coords_pal + 2 * 2 * 3 - 2)
up_params_loop:
    lodsw
    xchg ax, bx ; bx = shift value
    lodsw ; ax = amplitude or base value

    and ax, ax
    jns up_pl_nonneg
    neg ax
up_pl_nonneg:
    shr ax, 3
    add ax, 0x1000
    mov dx, ax
    shr dx, 7 ; step for ax

    mov di, (palette - 1)
    add di, cx

up_pl_fill_loop:
    inc bh
    js up_loop_dec
    add ax, dx
    jmp up_loop_cont
up_loop_dec:
    sub ax, dx
up_loop_cont:
    mov [di], ah

    add di, 3
    cmp di, (palette + pal_sz * 3)
    js up_pl_fill_loop

    loop up_params_loop

    ; load palette
    mov ax, 0x1012
    xor bx, bx
    mov cx, pal_sz
    mov dx, palette
    int 0x10
    ; update palette end

    cld
    
    ; redraw updated screen areas
    ; first iteration fills whole screen then changes to use blobs
    xor cx, cx
    mov dx, cx
    mov ax, 127
    mov bx, ax
    mov si, (var_coords_blobs + 3 * 2 * blob_num)
ds_jmp:
    jmp ds_fa_call

    mov si, var_coords_blobs
ds_blob_loop:

    ; generate coord loop limits
    lodsw
    xchg ax, cx ; cx: x = x_min
    lodsw
    xchg ax, dx ; dx: y = y_min
    lodsw ; sz

    ; transform sz in ax
    call corr_blob_sz

    mov bx, dx
    add bx, ax ; y_max
    add ax, cx ; x_max

    shr cx, 8
    shr dx, 8
    shr ax, 8
    shr bx, 8

ds_fa_call:
    
    ; fill area within 128x128 box with symmetrical extension
fa_fill_blob_loop:
    push ax

    push dx

fa_fill_blob_loop_inner:

    mov ax, cx
    rol ax, 1
    xor ax, cx
    xor ax, dx
    ror ax, 1
    xor ax, dx

    add ax, [pattern_frame]
    and ax, 0x007f

    mov ah, al
    shl ah, 1
    xor al, ah

    push cx
    push dx
    
    add cx, -128
    add dx, -128

fa_subdiv_loop:

    push dx

    ; transform cx & dx
    xor cx, 0x007f

fa_subdiv_loop_inner:
    xor dx, 0x007f

    push cx
    push dx

    add cx, 96
    add dx, 36

    and cx, cx
    js fa_subdiv_loop_cont
    cmp cx, 320
    jns fa_subdiv_loop_cont
    and dx, dx
    js fa_subdiv_loop_cont
    cmp dx, 200
    jns fa_subdiv_loop_cont

    mov ah, 0x0c
    int 0x10

fa_subdiv_loop_cont:

    pop dx
    pop cx

    sub dx, -128
    cmp dh, 1
    js fa_subdiv_loop_inner
    pop dx
    sub cx, -128
    cmp ch, 1
    js fa_subdiv_loop

    pop dx
    pop cx

    inc dx
    cmp bx, dx
    jns fa_fill_blob_loop_inner
    pop dx
    pop ax
    inc cx
    cmp ax, cx
    jns fa_fill_blob_loop
    ; fill area end

    cmp si, (var_coords_blobs + 3 * 2 * blob_num)

    jnz ds_blob_loop

    mov byte [ds_jmp + 1], 0 ; change function to use full loop now
    ; draw screen end

    pop cx

    pop di ; restore di to point to accels

    ; poll keyboard
    mov ax, 0x0100
    int 0x16
    jz wait_scr

    ; "restore" video mode
    mov ax, 0x0003
    int 0x10

    ; return to DOS
	mov	ax, 0x4c00
	int	0x21

    ; function for transforming blob size in ax into value for drawing
    ; arg and output: ax
corr_blob_sz:
    and ax, ax
    jns cbs_no_sign
    neg ax
cbs_no_sign:
    shr ax, 3
    add ax, (32768 / 10)
    ret

    ; function for filling word arrays (step always = 2)
    ; args: di - array start
    ;       cx - number of elements to fill
    ;       dx (when necessary) - limit for amplitude of random values
    ; cx, ax, si and di are volatile explicitly (with whatever function at bx uses)
    ; dir flag clear
    ; after first call changes to use abs-limiting
fill_word_array:
fwa_loop:
    ; upd RNG
    mov ax, [rng_state]
    
    mov si, ax
    shr si, 7
    xor ax, si
    mov si, ax
    shl si, 9
    xor ax, si
    mov si, ax
    shr si, 13
    xor ax, si

    mov [rng_state], ax
    ; upd RNG end

fwa_action_sel:
    jmp fwa_no_action_ptr
    ; abs-limited fill (abs < dx / 2)
    ; dx should be in a form of (2 ^ n - 1)
    push dx
    and ax, dx
    inc dx
    shr dx, 1
    sub ax, dx
    jns fwa_no_corr
    inc ax ; corrects to have equal number of + and - values -> add extra 0
fwa_no_corr:
    pop dx
    ; abs-limited fill end

fwa_no_action_ptr:
    stosw
    loop fwa_loop
    mov byte [fwa_action_sel + 1], 0 ; change function to use abs-limited correction
    ret

section .data
; empty

section .bss
rng_state:
    resw 1
pattern_frame:
    resw 1
pal_sz equ 256
palette:
    resb (pal_sz * 3)

; new vars (palette & blobs): {x_i, y_i, sz_i (i: 1 - 4)}, {a_c, s_c (c: r, g, b)}
pal_color_num equ 3
blob_num equ 4
param_num equ (3 * blob_num + 2 * pal_color_num)
var_coords_blobs:
    resw (3 * blob_num)
var_coords_pal:
    resw (2 * pal_color_num)
; blob velocities
    resw (3 * blob_num)
; palette velocities
    resw (2 * pal_color_num)
; blob accelerations
    resw (3 * blob_num)
; palette accelerations
    resw (2 * pal_color_num)
