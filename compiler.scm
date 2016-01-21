(define FIXNUM_SHIFT 2)
(define CHAR_SHIFT 8)
(define CHAR_TAG #xf)

(define (compile-program x)
	; converts immediate to binary format
	(define (immediate-rep x)
		(cond
			((integer? x) (arithmetic-shift x FIXNUM_SHIFT))
			((char? x)
				(bitwise-ior
					CHAR_TAG
					(arithmetic-shift (char->integer x) CHAR_SHIFT)
				)
			)
			((list? x)
				(if (atom? x)
					EMPTY_LIST
					1 ; TODO implement non-empty list
				)
			)
			((boolean? x)
				(if x
					#b10011111
					#b00011111
				)
			)
		)
	)

	; function header
	(print "	.text")
	(print "	.p2align 4")
	(print ".globl _scheme_entry")
	(print "_scheme_entry:")

	; function body
	(print "	movl $" (immediate-rep x) ", %eax")
	(print "	ret")
)

(compile-program #f)
