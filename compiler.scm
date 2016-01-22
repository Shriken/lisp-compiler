(define FIXNUM_SHIFT 2)
(define CHAR_SHIFT 8)
(define CHAR_TAG #xf)

(define (compile-program x)
	; converts an immediate to binary format
	(define (immediate-rep x)
		(cond
			((integer? x) (arithmetic-shift x FIXNUM_SHIFT))
			((char? x)
				(bitwise-ior
					CHAR_TAG
					(arithmetic-shift (char->integer x) CHAR_SHIFT)
				)
			)
			((null? x) EMPTY_LIST)
			((boolean? x)
				(if x
					#b10011111
					#b00011111
				)
			)
		)
	)

	(define (emit-expr x)
		(cond
			((immediate? x)
				(print "	movl $" (immediate-rep x) ", %eax")
			)
			((primcall? x)
				(case (primcall-op x)
					((add1)
						(emit-expr (primcall-operand1 x))
						(print "	addl $" (immediate-rep 1) ", %eax")
					)
					((sub1)
						(emit-expr (primcall-operand1 x))
						(print "	addl $" (immediate-rep -1) ", %eax")
					)
				)
			)
		)
	)

	(define (immediate? x)
		(or
			(integer? x)
			(char? x)
			(null? x)
			(boolean? x)
		)
	)

	(define (primcall? x)
		(and
			(not (atom? x))
			(map atom? x)
		)
	)

	(define primcall-op car)
	(define primcall-operand1 cadr)

	; function header
	(print "	.text")
	(print "	.p2align 4")
	(print ".globl _scheme_entry")
	(print "_scheme_entry:")

	; function body
	(emit-expr x)
	(print "	ret")
)

(compile-program `(sub1 (add1 5)))
