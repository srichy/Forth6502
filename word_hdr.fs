( -*- forth-asm -*- )

HEADLESSCODE

;; NAMELEN-dependent
HIGH_W .macro name_len, name, act=w_enter, flgs=0, prev
    .byte (\flgs << 7) | len(\name)
    .text format("%-7s", \name[:7])
    .addr \prev
dict:
cfa:
    jmp \act                  ; CFA
    .endmacro                   ; PFA implicit, follows macro

;; NAMELEN-dependent
CODE_W .macro name_len, name, flgs=0, prev
    .byte (\flgs << 7) | len(\name)
    .text format("%-7s", \name[:7])
    .addr \prev
dict:
cfa:
    .if proc=="65816"
    .ax16
    .endif
    .endmacro                   ; PFA implicit, follows macro

END-CODE
