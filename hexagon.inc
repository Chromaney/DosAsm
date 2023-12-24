field_w equ 17
field_h equ 8
field_sz equ (field_w * field_h)
base_x equ 15
base_y equ 28
step_x equ 18
step_y equ 22
hstep_y equ (step_y / 2)
hsize_x equ 13
hsize_y equ 12
; n_layers equ 8

; qimg: ;0  1  2  3  4  5  6  7    8    9    10   11   12
;     db 0, 0, 1, 2, 3, 4, 4, 5,   5,   6,   6,   7,   7   ; 0
;     db 0, 1, 2, 3, 3, 4, 5, 5,   6,   6,   7,   7,   255 ; 1
;     db 1, 1, 2, 3, 4, 4, 5, 5,   6,   6,   7,   7,   255 ; 2
;     db 2, 2, 3, 3, 4, 5, 5, 6,   6,   7,   7,   255, 255 ; 3
;     db 3, 3, 3, 4, 4, 5, 5, 6,   6,   7,   7,   255, 255 ; 4
;     db 4, 4, 4, 4, 5, 5, 6, 6,   7,   7,   255, 255, 255 ; 5
;     db 5, 5, 5, 5, 5, 5, 6, 6,   7,   7,   255, 255, 255 ; 6
;     db 5, 5, 5, 5, 5, 6, 6, 7,   7,   255, 255, 255, 255 ; 7
;     db 6, 6, 6, 6, 6, 6, 6, 7,   7,   255, 255, 255, 255 ; 8
;     db 6, 6, 6, 6, 6, 6, 7, 7,   255, 255, 255, 255, 255 ; 9
;     db 7, 7, 7, 7, 7, 7, 7, 7,   255, 255, 255, 255, 255 ; 10
;     db 7, 7, 7, 7, 7, 7, 7, 255, 255, 255, 255, 255, 255 ; 11

; offsets for starting positions of arrays for each id
qimg_d_idx:
    db 0, 8, 18, 30, 50, 76, 124, 184
; pairs: (y, x) -> when read as word: dh = x, dl = y
; stop pair: (255, 255)
qimg_d:
    db 0, 0, 0, 1, 1, 0, 255, 255 ; 8
    db 0, 2, 1, 1, 2, 0, 2, 1, 255, 255 ; 10
    db 0, 3, 1, 2, 2, 2, 3, 0, 3, 1, 255, 255 ; 12
    db 0, 4, 1, 3, 1, 4, 2, 3, 3, 2, 3, 3, 4, 0, 4, 1, 4, 2, 255, 255 ; 20
    db 0, 5, 0, 6, 1, 5, 2, 4, 2, 5, 3, 4, 4, 3, 4, 4, 5, 0, 5, 1, 5, 2, 5, 3, 255, 255 ; 26
    db 0, 7, 0, 8, 1, 6, 1, 7, 2, 6, 2, 7, 3, 5, 3, 6, 4, 5, 4, 6, 5, 4, 5, 5 ; continued
    db 6, 0, 6, 1, 6, 2, 6, 3, 6, 4, 6, 5, 7, 0, 7, 1, 7, 2, 7, 3, 7, 4, 255, 255 ; 48
    db 0, 9, 0, 10, 1, 8, 1, 9, 2, 8, 2, 9, 3, 7, 3, 8, 4, 7, 4, 8, 5, 6, 5, 7 ; continued
    db 6, 6, 6, 7, 7, 5, 7, 6, 8, 0, 8, 1, 8, 2, 8, 3, 8, 4, 8, 5, 8, 6 ; continued
    db 9, 0, 9, 1, 9, 2, 9, 3, 9, 4, 9, 5, 255, 255 ; 60
    db 0, 11, 0, 12, 1, 10, 1, 11, 2, 10, 2, 11, 3, 9, 3, 10, 4, 9, 4, 10 ; continued
    db 5, 8, 5, 9, 6, 8, 6, 9, 7, 7, 7, 8, 8, 7, 8, 8, 9, 6, 9, 7 ; continued
    db 10, 0, 10, 1, 10, 2, 10, 3, 10, 4, 10, 5, 10, 6, 10, 7 ; continued
    db 11, 0, 11, 1, 11, 2, 11, 3, 11, 4, 11, 5, 11, 6, 255, 255 ; 72

; offsets for starting positions in adjacency array adj_data + "after end" index
adj_data_idx:
    db 0, 6, 9, 14, 14, 18, 20, 23, 23, 27, 29, 32, 32, 32, 32, 32 ; even cols
    db 32, 38, 43, 46, 46, 50, 53, 55, 55, 55, 55, 55, 55, 55, 55, 55 ; odd cols
    db 55 ; "after end"

; indexing: 0b000olrtb (o - odd column, l/r/t/b - on left/right/top/bottom edge)
; values - 1D-shifts (signed byte) in cell array
; some combinations of indices are impossible or considered so (1-cell width/height)
adj_data:
    ; even: up, down,  left up,    left down, right up,  right down
    db      -1,    1, -field_h, -field_h + 1,  field_h, field_h + 1 ; 6 values | cumul: 6 | 00000 - even column, middle cell
    db      -1,       -field_h,                field_h              ; 3 values | cumul: 9 | 00001 - even column, bottom edge
    db             1, -field_h, -field_h + 1,  field_h, field_h + 1 ; 5 values | cumul: 14 | 00010 - even column, top edge
                                                                    ; 0 values | cumul: 14 | 00011 - impossible
    db      -1,    1, -field_h, -field_h + 1                        ; 4 values | cumul: 18 | 00100 - even column, right edge
    db      -1,       -field_h                                      ; 2 values | cumul: 20 | 00101 - even column, right bottom corner
    db             1, -field_h, -field_h + 1                        ; 3 values | cumul: 23 | 00110 - even column, right top corner
                                                                    ; 0 values | cumul: 23 | 00111 - impossible
    db      -1,    1,                          field_h, field_h + 1 ; 4 values | cumul: 27 | 01000 - even column, left edge
    db      -1,                                field_h              ; 2 values | cumul: 29 | 01001 - even column, left bottom corner
    db             1,                          field_h, field_h + 1 ; 3 values | cumul: 32 | 01010 - even column, left top corner
                                                                    ; 0 values | cumul: 32 | 01011 - impossible
                                                                    ; 0 values | cumul: 32 | 01100 - impossible
                                                                    ; 0 values | cumul: 32 | 01101 - impossible
                                                                    ; 0 values | cumul: 32 | 01110 - impossible
                                                                    ; 0 values | cumul: 32 | 01111 - impossible

    ; odd: up, down,      left up, left down,    right up, right down
    db     -1,    1, -field_h - 1,  -field_h, field_h - 1,    field_h ; 6 values | cumul: 38 | 10000 - odd column, middle cell
    db     -1,       -field_h - 1,  -field_h, field_h - 1,    field_h ; 5 values | cumul: 43 | 10001 - odd column, bottom edge
    db            1,                -field_h,                 field_h ; 3 values | cumul: 46 | 10010 - odd column, top edge
                                                                      ; 0 values | cumul: 46 | 10011 - impossible
    db     -1,    1, -field_h - 1,  -field_h                          ; 4 values | cumul: 50 | 10100 - odd column, right edge
    db     -1,       -field_h - 1,  -field_h                          ; 3 values | cumul: 53 | 10101 - odd column, right bottom corner
    db            1,                -field_h                          ; 2 values | cumul: 55 | 10110 - odd column, right top corner
                                                                      ; 0 values | cumul: 55 | 10111 - impossible
                                                                      ; 0 values | cumul: 55 | 11000 - impossible with current setup
                                                                      ; 0 values | cumul: 55 | 11001 - impossible with current setup
                                                                      ; 0 values | cumul: 55 | 11010 - impossible with current setup
                                                                      ; 0 values | cumul: 55 | 11011 - impossible
                                                                      ; 0 values | cumul: 55 | 11100 - impossible
                                                                      ; 0 values | cumul: 55 | 11101 - impossible
                                                                      ; 0 values | cumul: 55 | 11110 - impossible
                                                                      ; 0 values | cumul: 55 | 11111 - impossible
