(define FIXNUM_MASK #x3)
(define FIXNUM_TAG #x0)
(define FIXNUM_SHIFT #x2)

(define CHAR_MASK #xff)
(define CHAR_TAG #xf)
(define CHAR_SHIFT #x8)

(define EMPTY_LIST #x2f)

(define BOOL_MASK #x7f)
(define BOOL_TAG #x1f)
(define BOOL_SHIFT #x7)

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
					(bitwise-ior #x80 BOOL_TAG)
					(bitwise-ior #x00 BOOL_TAG) ; the ior with 0 is just for readability
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
				(define (emit-cmp bin-expr)
					(print "	cmpl $" bin-expr ", %eax")
					(print "	movl $0, %eax")
					(print "	sete %al")
					(print "	sall $" BOOL_SHIFT ", %eax")
					(print "	orl $" BOOL_TAG ", %eax")
				)

				(define (emit-type-check-primcall type-mask type-tag x)
					(emit-expr (primcall-operand1 x))
					(print "	andl $" type-mask ", %eax")
					(emit-cmp type-tag)
				)

				(case (primcall-op x)
					((add1)
						(emit-expr (primcall-operand1 x))
						(print "	addl $" (immediate-rep 1) ", %eax")
					)
					((sub1)
						(emit-expr (primcall-operand1 x))
						(print "	addl $" (immediate-rep -1) ", %eax")
					)
					((integer->char)
						(emit-expr (primcall-operand1 x))
						(print "	sal $" (- CHAR_SHIFT FIXNUM_SHIFT) ", %eax")
						(print "	orl $" CHAR_TAG ", %eax")
					)
					((char->integer)
						(emit-expr (primcall-operand1 x))
						(print "	sar $" (- CHAR_SHIFT FIXNUM_SHIFT) ", %eax")
					)
					((null?)
						(emit-expr (primcall-operand1 x))
						(emit-cmp EMPTY_LIST)
					)
					((integer?) (emit-type-check-primcall FIXNUM_MASK FIXNUM_TAG x))
					((boolean?) (emit-type-check-primcall BOOL_MASK BOOL_TAG x))
					((char?) (emit-type-check-primcall CHAR_MASK CHAR_TAG x))

					(else
						(print "primcall " (primcall-op x) " not recognized")
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
			(map
				(lambda (x)
					(or
						(atom? x)
						(primcall? x)
					)
				)
				x
			)
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

(compile-program `(boolean? (integer->char (char->integer #\c))))
