     .section    __TEXT,__text,regular,pure_instructions
     .globl  _scheme_entry
     .p2align    4, 0x90
_scheme_entry:
     .cfi_startproc
     movl    $-2147483648, %eax
     retq
     .cfi_endproc
