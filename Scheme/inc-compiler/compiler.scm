(load "test-driver.scm")
(load "tests-1.2-req.scm")
(load "tests-1.1-req.scm")

(define wordsize       4)
(define fxshift        2)
(define chshift        8)
(define fxmask      #x03)
(define fxtag       #x00)
(define chtag       #x0F)
(define bool_t      #x6F)
(define bool_f      #x2F)
(define nil_list    #x3F)

(define fixnum-bits (- (* wordsize 8) fxshift))
(define fxlower (- (expt 2 (- fixnum-bits 1))))
(define fxupper (sub1 (expt 2 (- fixnum-bits 1))))
(define (fixnum? x)
  (and (integer? x) (exact? x) (<= fxlower x fxupper)))

(define (immediate? x)
  (or (fixnum? x) (boolean? x) (char? x) (null? x)))

(define (process-char x)
  (bitwise-ior (ash (char->integer x) chshift) chtag))

(define (immediate-rep x)
  (cond
    [(fixnum? x) (ash x fxshift)]
    [(null? x) nil_list]
    [(boolean? x) (if x bool_t bool_f)]
    [(char? x) (process-char x)]
    [else #f]))

(define (emit-program x)
  (unless (immediate? x) (error 'emit-program "Not an integer"))
  (emit "     .section    __TEXT,__text,regular,pure_instructions")
  (emit "     .globl  _scheme_entry")
  (emit "     .p2align    4, 0x90")
  (emit "_scheme_entry:")
  (emit "     .cfi_startproc")
  (emit "     movl    $~s, %eax" (immediate-rep x))
  (emit "     retq")
  (emit "     .cfi_endproc"))
