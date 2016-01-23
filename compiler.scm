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

(define WORD_SIZE 8)

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

	(define (emit-expr x stack-index)
		(cond
			((immediate? x)
				(print "	movq $" (immediate-rep x) ", %rax")
			)
			((primcall? x) (emit-primcall x stack-index))
		)
	)

	(define (emit-primcall x stack-index)
		(define (emit-cmp bin-expr)
			(print "	cmpq $" bin-expr ", %rax")
			(print "	movq $0, %rax")
			(print "	sete %al")
			(print "	salq $" BOOL_SHIFT ", %rax")
			(print "	orq $" BOOL_TAG ", %rax")
		)

		(define (emit-type-check-primcall type-mask type-tag x)
			(emit-expr (primcall-operand1 x) stack-index)
			(print "	andq $" type-mask ", %rax")
			(emit-cmp type-tag)
		)

		(case (primcall-op x)
			; unary primcalls
			((add1)
				(emit-expr (primcall-operand1 x) stack-index)
				(print "	addq $" (immediate-rep 1) ", %rax")
			)
			((sub1)
				(emit-expr (primcall-operand1 x) stack-index)
				(print "	addq $" (immediate-rep -1) ", %rax")
			)
			((integer->char)
				(emit-expr (primcall-operand1 x) stack-index)
				(print "	salq $" (- CHAR_SHIFT FIXNUM_SHIFT) ", %rax")
				(print "	orq $" CHAR_TAG ", %rax")
			)
			((char->integer)
				(emit-expr (primcall-operand1 x) stack-index)
				(print "	sar $" (- CHAR_SHIFT FIXNUM_SHIFT) ", %rax")
			)
			((null?)
				(emit-expr (primcall-operand1 x) stack-index)
				(emit-cmp EMPTY_LIST)
			)
			((integer?) (emit-type-check-primcall FIXNUM_MASK FIXNUM_TAG x))
			((boolean?) (emit-type-check-primcall BOOL_MASK BOOL_TAG x))
			((char?) (emit-type-check-primcall CHAR_MASK CHAR_TAG x))

			; binary primcalls
			((+)
				; save arg 2 to the stack
				(emit-expr (primcall-operand2 x) stack-index)
				(print "	movq %rax, " stack-index "(%rsp)")
				(emit-expr
					(primcall-operand1 x)
					(- stack-index WORD_SIZE)
				)
				(print "	addq " stack-index "(%rsp), %rax")
			)

			(else
				(print "primcall " (primcall-op x) " not recognized")
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
	(define primcall-operand2 caddr)

	; function header
	(print "	.text")
	(print "	.p2align 4")
	(print ".globl _scheme_entry")
	(print "_scheme_entry:")

	; function body
	(emit-expr x (- WORD_SIZE))
	(print "	ret")
)

(compile-program `(boolean? (integer? (+ (+ 3 4) 7))))
