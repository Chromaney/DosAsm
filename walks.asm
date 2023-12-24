; procedures/functions should preserve ax, bx, cx, dx
; si, di are used as temporary storage in each proc/fcn

org 0x100
section .text

; constants
frame_div equ 3
worker_num equ 3

start:
    ; set video mode
    mov ax, 0x0013
    int 0x10

    ; set palette
    mov ax, 0x1012
    mov bx, 0
    mov cx, pal_sz
    mov dx, palette
    int 0x10

    ; expand animations into runtime memory
    mov bx, anim_init_data_idx
    mov cx, anim_init_data
    mov dx, anim_init_data_exp
    call expand_anim_data
    mov bx, anim_expand_data_idx
    mov cx, anim_expand_data
    mov dx, anim_expand_data_exp
    call expand_anim_data
    mov bx, anim_fade_data_idx
    mov cx, anim_fade_data
    mov dx, anim_fade_data_exp
    call expand_anim_data

    call reset_field

    ; init frame counter
    mov cl, 0

    mov ch, 0

    ; wait for next frame
wait_scr:
    mov dx, 0x03da
    in al, dx
    test al, 8
    jz wait_scr

    ; change frame counter
    inc cl
    cmp cl, frame_div
    jnz kbrd_poll
    mov cl, 0
    
    ; update every frame_div frames
    call upd_field

kbrd_poll:
    ; poll keyboard
    push ax
    mov ax, 0x0100
    int 0x16
    pop ax
    jz wait_scr

    call audio_off

    ; "restore" video mode
    mov ax, 0x0003
    int 0x10

    ; return to DOS
    mov	ax, 0x4c00
    int	0x21

    ; function for playing a note indefinitely
    ; cx - note id
play_note:
    ret ; remove for bad sounds
    push ax
    push bx
    push cx
    push dx

    mov bx, note_cnt
    shl cx, 1
    add bx, cx
    mov cx, [bx]

    mov dx, 0x0043
    mov al, 0xb6
    out dx, al
    mov ax, cx
    mov dx, 0x0042
    out dx, al
    shr ax, 8
    out dx, al

    mov dx, 0x0061
    in al, dx
    or al, 0b00000011
    out dx, al

    pop dx
    pop cx
    pop bx
    pop ax
    ret

    ; function for turning off audio
    ; no inputs
audio_off:
    push ax
    push dx

    mov dx, 0x0061
    in al, dx
    and al, 0b11111100
    out dx, al

    pop dx
    pop ax
    ret

    ; rng function
next_rand:
    push ax
    push cx ; used as output in int 0x1a
    push dx ; used as output in int 0x1a

    mov ax, [rng_state]
    cmp ax, 0
    jnz nr_upd_state

    mov ax, 0x0000
    int 0x1a
    mov ax, dx
    cmp ax, 0
    jnz nr_upd_state
    inc ax

nr_upd_state:
    mov dx, ax
    shr dx, 7
    xor ax, dx
    mov dx, ax
    shl dx, 9
    xor ax, dx
    mov dx, ax
    shr dx, 13
    xor ax, dx

    mov [rng_state], ax

    pop dx
    pop cx
    pop ax
    ret

    ; function for getting an array of non-repeating random bytes from 0 to upper limit
    ; random numbers are added instead of being replaced to smooth out "(mod limit)" imbalances
    ; bx - start of array
    ; cx - number of bytes; ch ignored
    ; dx - upper limit (excluded); dh = 0
gen_rand_arr:
    push ax
    push bx
    push cx

    mov di, bx ; remember array start in di
    mov ch, cl ; remember full number in ch
    mov ax, 0

gra_loop:
    call next_rand
    add ax, [rng_state]
    mov ah, 0
gra_limit_loop:
    cmp ax, dx ; word-wide to avoid sign issues
    js gra_in_limit
    sub ax, dx
    jmp gra_limit_loop
gra_in_limit:
    ; checking for repeats with previous numbers
    mov si, bx
gra_unique_loop:
    dec si
    cmp si, di
    js gra_is_unique
    cmp al, [si]
    jz gra_loop
    jmp gra_unique_loop
gra_is_unique:
    mov [bx], al
    inc bx
    dec cl
    jnz gra_loop

    pop cx
    pop bx
    pop ax
    ret

    ; function for expanding animation data for quick indexing
    ; bx - index table base
    ; cx - animation source data base
    ; dx - animation destination data base
expand_anim_data:
    push ax
    push dx
    ; ah - frame index, al - layer index
    mov ax, 0
    ; di - destination for data
    mov di, dx
ead_layer_loop:
    push ax
    xlat
    mov ah, 0
    ; si - source for data
    mov si, cx
    add si, ax
    pop ax

    mov ah, 0
ead_frame_loop:
    cmp ah, anim_frames
    jz ead_end_frame_loop
    mov dx, [si]
    add si, 2
    add ah, dl
    mov [di], dh
    inc di
ead_frame_fill_loop:
    dec dl
    jz ead_end_frame_fill_loop
    mov byte [di], 0xff
    inc di
    jmp ead_frame_fill_loop
ead_end_frame_fill_loop:
    jmp ead_frame_loop

ead_end_frame_loop:
    inc al
    cmp al, anim_layers
    jnz ead_layer_loop

    pop dx
    pop ax
    ret

    ; field reset function
    ; inputs are same every time so no need to provide any
reset_field:
    push ax
    push bx
    push cx
    push dx

    mov bx, worker_pos
    mov cx, worker_num
    mov dh, 0
    mov dl, field_sz
    call gen_rand_arr

    mov ax, 0x0000 ; and mask - reset all
    mov dx, 0x8000 ; or mask - set bit 15 (a) only
    call activate_field

    pop dx
    pop cx
    pop bx
    pop ax
    ret

    ; field staleness check function
    ; inputs are same every time so no need to provide any
check_activate_field:
    push ax
    push bx
    push cx
    push dx

    mov bx, field
    mov cx, 0 ; total counter
    mov si, 0x0000 ; or-"accumulator" for active flags (0b_a0sspppp_ffffffff)
    mov di, 0xffff ; and-"accumulator" for active flags (0b_a0sspppp_ffffffff)
caf_full_loop:
    or si, [bx]
    and di, [bx]
    add bx, 2
    inc cx
    cmp cx, field_sz
    jnz caf_full_loop

    test si, 0x8000 ; do nothing if active hexes are present
    jnz caf_ret

    and di, 0x3f00 ; keep 00sspppp in high byte
    cmp di, 0x3000 ; reset field when each ss = 3
    js caf_check_stage
    call reset_field
    jmp caf_ret

caf_check_stage:
    mov ax, 0xffff ; and mask - no change
    mov dx, 0x8000 ; or mask - set bit 15 (a) only
    and si, 0x3000 ; check if ss is consistent and put into cx if so
    and di, 0x3000
    mov cx, 0xffff
    cmp si, di
    jnz caf_call_act
    mov cx, si
    shr cx, 12
caf_call_act:
    call activate_field

caf_ret:
    pop dx
    pop cx
    pop bx
    pop ax
    ret

    ; field activation function (simultaneous)
    ; ax - and mask for values
    ; dx - or mask for values (applied after and mask)
    ; cx - new animation stage number
activate_field:
    push ax
    push bx
    push dx
    push cx

    ; turn off audio after avery stage
    call audio_off

    cmp cx, 1 ; check if partial activation needed
    jnz af_no_rm_act_bit
    and dx, 0x7fff ; remove activation bit

af_no_rm_act_bit:
    mov bx, field
    mov cx, 0 ; total counter
af_full_loop:
    and [bx], ax
    or [bx], dx
    add bx, 2
    inc cx
    cmp cx, field_sz
    jnz af_full_loop

    pop cx
    push cx ; restore cx to animation stage number

    cmp cx, 1 ; check if partial activation needed
    jnz af_ret

    mov cx, 0 ; worker counter
    mov al, 0 ; anim start delay counter
af_part_act_loop:
    mov bx, worker_pos
    add bx, cx
    mov bl, [bx] ; bx = pos
    mov bh, 0
    shl bx, 1
    add bx, field ; bx = pointer to active cell entry
    mov dl, al
af_rand_pal_loop:
    call next_rand
    mov dh, [rng_state]
    and dh, 0x0f
    cmp dh, 0
    jz af_rand_pal_loop

    push cx
    mov cl, dh
    mov ch, 0
    call play_note
    pop cx

    or dh, 0b10010000 ; active, anim stage #2 (ss = 1)
    mov [bx], dx
    sub al, anim_delay_timer
    inc cx
    cmp cx, worker_num
    jnz af_part_act_loop

af_ret:
    pop cx
    pop dx
    pop bx
    pop ax
    ret

    ; field update function
    ; inputs are same every time so no need to provide any
upd_field:
    push ax
    push bx
    push cx
    push dx

    mov bx, field
    mov cl, 0 ; x-counter
    mov dl, 0 ; y-counter
uf_full_loop:
    mov ax, [bx] ; ax = 0b_a0sspppp_ffffffff
    call draw_hex
    test ah, 0x80
    jz uf_full_loop_cont
    mov si, ax
    and si, 0b00111111_00000000 ; animation stage + subpalette id
    add si, 0b00010000_00000000 ; next animation stage
    cmp al, 127
    jz uf_full_loop_next_stage
    inc al
    jmp uf_full_loop_cont

uf_full_loop_next_stage:
    mov ax, si

uf_full_loop_cont:
    mov [bx], ax
    mov si, ax ; backup
    and ah, 0xf0
    cmp ah, 0b10010000
    jnz uf_full_loop_iter_fin
    cmp al, anim_advance_timer
    jnz uf_full_loop_iter_fin

    mov ax, si
    and ah, 0x0f ; subpalette id
    call next_rand
    mov al, [rng_state]
    and al, 0x01 ; max +2 step in palette
    inc al
    add ah, al
    and ah, 0x0f ; ah is new palette id (corrected for no-zero below)
    cmp ah, 0
    jnz uf_no_pal_inc
    inc ah
uf_no_pal_inc:
    mov al, 0x00 ; position flags
    cmp dl, (field_h - 1) ; test if bottom row
    jnz uf_chk_not_b
    or al, 0x01
uf_chk_not_b:
    cmp dl, 0 ; test if top row
    jnz uf_chk_not_t
    or al, 0x02
uf_chk_not_t:
    cmp cl, (field_w - 1) ; test if right column
    jnz uf_chk_not_r
    or al, 0x04
uf_chk_not_r:
    cmp cl, 0 ; test if left column
    jnz uf_chk_not_l
    or al, 0x08
uf_chk_not_l:
    test cl, 0x01 ; test if odd column
    jz uf_chk_end
    or al, 0x10
uf_chk_end:
    push dx ; backup

    push bx
    mov bx, adj_data_idx
    push ax
    mov ah, 0
    add bx, ax
    pop ax
    mov bx, [bx] ; bl = start idx, bh = end idx (in adj_data)
    mov dx, bx ; backup dx with adj_data indices
    mov di, adj_data
    mov ch, bh
    sub ch, bl ; counter for possible directions (starts at total number)
    shr bx, 8 ; bh = 0, bl = prev bh
    add di, bx ; points after the last possible direction
    mov bx, dx ; restore bx with adj_data indices
    mov si, adj_data
    mov bh, 0
    add si, bx ; points at the first possible direction (for wrap-around purposes)
uf_dir_gen_loop:
    push si
    push di
    call next_rand
    pop di
    pop si
    mov bx, [rng_state]
    and bx, 0x0007 ; zero bh and keep last three bits of bl (never > 8 directions)
    cmp bl, ch
    jns uf_dir_gen_loop
    sub di, bx ; shift starting direction by random number
    pop bx ; restore to pointer to current cell

    push bx
    sub dh, dl ; dh = total number of directions
    shr dx, 8 ; dx = total number of directions
uf_dir_chk_loop:
    dec di
    cmp di, si
    jns uf_dir_no_wrap
    add di, dx ; move di to the end of data chunk
uf_dir_no_wrap:
    push bx ; backup pointer to current cell
    push dx ; backup wrap correction value
    mov dx, [di]
    mov dh, 0
    cmp dl, 0
    jns uf_dir_nonneg
    mov dh, 0xff
uf_dir_nonneg:
    sal dx, 1
    add bx, dx ; now pointer to a potential cell to move to
    pop dx
    cmp word [bx], 0b00010000_00000000
    jnz uf_dir_chk_loop_nm ; do not move into active (now or in the past) cell
    
    ; move into next cell
    ; corrsponding note
    push cx
    mov cl, ah
    and cl, 0x0f
    mov ch, 0
    call play_note
    pop cx

    ; activate next cell with correct state
    or ah, 0b10010000 ; add status to the palette id from before
    mov al, 0xff ; delay till the next frame for the newly activated cells
    ; ^ introduces asymmetry when updated later on current frame (not guaranteed), 
    ; but too difficult to create "proper" behavior
    mov [bx], ax
    jmp uf_next_cell_found
uf_dir_chk_loop_nm:
    pop bx
    dec ch
    cmp ch, 0
    jns uf_dir_chk_loop

    ; next cell not found -> reinitialize worker with a new random place
    mov dx, (2 * field_sz)
    shr dx, 1
    add bx, dx ; move starting position to the other side of the field
    call next_rand
    mov dl, [rng_state]
    and dl, 0x7f ; keep unsigned value
    shl dl, 1
    mov dh, 0
    add bx, dx
uf_rand_rest_loop:
    cmp bx, field
    js uf_rand_rest_loop_end
    sub bx, (2 * field_sz)
    jmp uf_rand_rest_loop
uf_rand_rest_loop_end:
    add bx, (2 * field_sz)
    mov di, field
    add di, (2 * field_sz) ; wrap-around threshold
    mov si, bx ; end threshold

    ; loop over field from bx with wrap-around and move worker to the first available cell
uf_find_avail_loop:
    cmp word [bx], 0b00010000_00000000
    jnz uf_not_avail
    ; move worker here with random palette
uf_pal_gen_loop:
    push si
    push di
    call next_rand
    pop di
    pop si
    mov dh, [rng_state]
    and dh, 0x0f
    cmp dh, 0
    jz uf_pal_gen_loop
    or dh, 0b10010000
    mov dl, 0xff
    mov [bx], dx
    jmp uf_no_avail_cells ; double purpose label - go there also if next cell found
uf_not_avail:
    add bx, 2
    cmp bx, si
    jz uf_no_avail_cells ; end loop if all cells are checked
    cmp bx, di
    js uf_find_avail_loop
    sub bx, (2 * field_sz)
    jmp uf_find_avail_loop

uf_next_cell_found:
    pop bx
uf_no_avail_cells:
    pop bx
    pop dx
uf_full_loop_iter_fin:
    add bx, 2
    inc dh
    inc dl
    cmp dl, field_h
    jnz uf_full_loop
    mov dl, 0
    inc cl
    cmp cl, field_w
    jnz uf_full_loop

    call check_activate_field

    pop dx
    pop cx
    pop bx
    pop ax
    ret

    ; hexagon drawing function
    ; ax - cell state (0b_a0sspppp_ffffffff)
    ; cl - x_cell_id
    ; dl - y_cell_id
draw_hex:
    test al, 0x80
    jnz dh_ret_npop

    push cx
    push dx

    push ax
    mov al, step_y
    mul dl
    mov dx, ax
    add dx, base_y
    mov ah, cl
    and ah, 0x01
    jz dh_no_y_corr
    sub dx, hstep_y
dh_no_y_corr:
    mov al, step_x
    mul cl
    mov cx, ax
    add cx, base_x
    pop ax 

    push ax
    and ah, 0x3f ; ah = 0b00sspppp

    push bx
    cmp ah, 0x10
    js dh_init_state
    cmp ah, 0x20
    js dh_expand_state
    cmp ah, 0x30
    js dh_fade_state
    jmp dh_ret

dh_init_state:
    mov bx, anim_init_data_exp
    jmp dh_fill

dh_expand_state:
    mov bx, anim_expand_data_exp
    jmp dh_fill

dh_fade_state:
    mov bx, anim_fade_data_exp
    jmp dh_fill

dh_fill:
    push ax
    mov ah, 0
    add bx, ax
    pop ax
    and ah, 0x0f
dh_layer_loop:
    push ax ; ax = 0b_llllpppp_0fffffff (l - layer id)

    mov si, [bx]
    and si, 0x00ff ; si = 0b_00000000_iiiiiiii (i - subpalette index)
    cmp si, 0xff
    jz dh_skip_draw

    shr ax, 4
    and ax, 0x0ff0
    or ax, si ; ax = 0b_0000llll_ppppiiii (i in regular cases is <= 15)

    call draw_hex_layer
dh_skip_draw:
    pop ax
    add bx, anim_frames
    add ah, 0x10
    jns dh_layer_loop ; 8 layers -> ah = 0x8? is next after final -> cmp with 0 gives < 0

dh_ret:
    pop bx
    pop ax
    pop dx
    pop cx
dh_ret_npop:
    ret

    ; hexagon layer drawing function
    ; ah - layer id, al - palette color
    ; cx - x_center
    ; dx - y_center
draw_hex_layer:
    push bx
    push ax
    xchg al, ah
    mov ah, 0
    mov bx, qimg_d_idx
    xlat
    mov bx, qimg_d
    add bx, ax
    pop ax

dhl_px_loop:
    mov si, [bx]
    cmp si, 0xffff
    jz dhl_ret

    ; si - delta_y, di - delta_x (from si_h = delta_x, si_l = delta_y)
    mov di, si
    shr di, 8
    and si, 0x00ff

    push ax
    mov ah, 0x0c

    add cx, di
    add dx, si
    int 0x10
    sub cx, di
    sub cx, di
    int 0x10
    sub dx, si
    sub dx, si
    int 0x10
    add cx, di
    add cx, di
    int 0x10
    sub cx, di
    add dx, si

    pop ax

    add bx, 2
    jmp dhl_px_loop

dhl_ret:
    pop bx
    ret

section .data
%include "palette.inc"
%include "hexagon.inc"
%include "anims.inc"
rng_state:
    dw 0x0000

; 0 - filler frequency, 1 through 15 - three octaves of pentatonic major scale
; notes: C3, 
;        C4, D4, E4, G4, A4, 
;        C5, D5, E5, G5, A5, 
;        C6, D6, E6, G6, A6
; freqs:  130.81, 
;         261.63,  293.66,  329.63,  392.00,  440.00, 
;         523.25,  587.33,  659.26,  783.99,  880.00, 
;        1046.50, 1174.66, 1328.51, 1567.98, 1760.00
; counter = 1_193_180 / frequency
note_cnt:
    dw 9121
    dw 4561, 4063, 3620, 3044, 2712
    dw 2280, 2032, 1810, 1522, 1356
    dw 1140, 1016,  905,  761,  678


section .bss
; field - each "cell": word 0b_a0sspppp_ffffffff
; a - is active, s - animation stage (0, 1, 2, 3 - init, expand, fade, none), 
; p - subpalette id, f - animation frame (0 to 127; if >= 128 then ignored)

; field "shape"
;        ___       ___
;       /   \     /   \
;   ___/ x=1 \___/ x=3 \___
;  /   \ y=0 /   \ y=0 /   \
; / x=0 \___/ x=2 \___/ x=4 \
; \ y=0 /   \ y=0 /   \ y=0 /
;  \___/ x=1 \___/ x=3 \___/
;  /   \ y=1 /   \ y=1 /   \
; / x=0 \___/ x=2 \___/ x=4 \
; \ y=1 /   \ y=1 /   \ y=1 /
;  \___/ x=1 \___/ x=3 \___/
;  /   \ y=2 /   \ y=2 /   \
; / x=0 \___/ x=2 \___/ x=4 \
; \ y=2 /   \ y=2 /   \ y=2 /
;  \___/ x=1 \___/ x=3 \___/
;  /   \ y=3 /   \ y=3 /   \
; / x=0 \___/ x=2 \___/ x=4 \
; \ y=3 /   \ y=3 /   \ y=3 /
;  \___/     \___/     \___/

field:
    resw field_sz
worker_pos:
    resb worker_num
anim_init_data_exp:
    resb anim_data_exp_sz
anim_expand_data_exp:
    resb anim_data_exp_sz
anim_fade_data_exp:
    resb anim_data_exp_sz
