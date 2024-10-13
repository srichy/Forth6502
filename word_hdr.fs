( -*- forth-asm -*- )

HEADLESSCODE

;; NAMELEN-dependent
HIGH_W .macro name_len, name, act=w_enter, flgs=0, prev
    .text \name
    .byte (\flgs << 7) | len(\name)
    .addr \prev
cfa:
    jmp \act                  ; CFA
    .endmacro                   ; PFA implicit, follows macro

;; NAMELEN-dependent
CODE_W .macro name_len, name, flgs=0, prev
    .text \name
    .byte (\flgs << 7) | len(\name)
    .addr \prev
cfa:
    .if proc=="65816"
    .ax16
    .endif
    .endmacro                   ; PFA implicit, follows macro

END-CODE
