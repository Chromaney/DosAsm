; video mode not restored upon exit - use cls in DOS afterwards
; exit by pressing ESC

org 0x100
section .text

start:
    ; set video mode, clear video memory
    mov al, 0x13 ; ax = 0x0000 at start
    int 0x10

    ; point ds:si and es:di to video memory
    push word 0xa000
    pop es
    push es
    pop ds

    ; init palette
    add di, 0x0122 ; 32 bytes in front of si ; at start, si = 0x0100, di = 0xfffe
    ; ^ G is 32 steps behind R/B

    mov dx, 0x03c9 ; out port
    inc cx ; now 0x0100
fill_pal_loop:
    mov ax, cx
    neg al
    jns fpl_nonneg
    xor al, al ; zero out values when al > 127
fpl_nonneg:

    test al, 0b01000000
    jz fpl_no_corr
    xor al, 0b01111111 ; correct sum to /\ shape
fpl_no_corr:
    sub al, [di - 32] ; use previous values for /^\_ shape
    ; ^ could use just si, but then "random" shapes are worse

    stosb
    out dx, al
    outsb
    out dx, al

    loop fill_pal_loop

    ; main loop
    ; bp - horizontal shift var (and more)
main_loop:
    
    ; draw screen
    mov dl, -1 ; screen row
    xor di, di ; write to
draw_loop: ; looping rows
    xor si, si ; read always from first line
    mov cx, 320
    mov dh, (200 - 64 / 2 - 24) ; starting mountain height

    inc dl
    lahf

draw_loop_inner: ; single row loop - looping columns
    push bp
    lodsb

    sahf
    jz dli_skip_bp0
    xor bp, bp
dli_skip_bp0:

    add bp, si ; add column position (or set to it for mountains)
    and bp, 0x0078 ; limit range and repeat values 8 times in a row
    mov bx, [bp + start] ; ss still points to original segment ; "random" values

    ; common processing of random numbers
    and bx, 0x020f ; bl - for aurora pattern, bh - for mountains (processed differently)
    dec bh
    add bl, al
    add dh, bh

    sahf
    jz dli_first
    js dli_btm
    add al, dl
    jmp dli_end

dli_first: ; first line - base for aurora pattern
    add al, al
    add al, bl
    shr al, 2 ; "average" new and previous values ((3 * old + 1 * new) / 4)
    jmp dli_end

dli_btm: ; mountains + reflection
    sub al, dl
    sub al, 8 ; reflection

    cmp dh, dl
    jns dli_end
    xor al, al ; black color if inside mountains

dli_end:

    stosb
    pop bp

    loop draw_loop_inner
    cmp dl, 200 ; dl is updated at the beginning of the loop
    jnz draw_loop

    ; update horizontal shift for next frame
    sub bp, 85

    ; poll keyboard for ESC
    in al, 0x60
    dec al
    jnz main_loop

    ; return to DOS
    ret
