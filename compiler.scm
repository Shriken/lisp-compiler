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

(define PAIR_MASK #x7)
(define PAIR_TAG #x1)

(define VECTOR_MASK #x7)
(define VECTOR_TAG #x2)

(define STRING_MASK #x7)
(define STRING_TAG #x3)

(define SYMBOL_MASK #x7)
(define SYMBOL_TAG #x5)

(define WORD_SIZE 8)

(define (compile-program x)
	(define (emit-expr x stack-index env)
		(cond
			((immediate? x)
				(print "	movq $" (immediate-rep x) ", %rax")
			)
			((variable? x)
				(print "	movq " (lookup x env) "(%rsp), %rax")
			)
			((if? x) (emit-if (test x) (conseq x) (altern x) stack-index env))
			((let? x) (emit-let (bindings x) (body x) stack-index env))
			((primcall? x) (emit-primcall x stack-index env))
		)
	)

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

	(define variable? symbol?)
	(define (lookup expr env)
		(if (null? env)
			; if there's nothing in the env, throw an error
			(print "variable not bound: " expr)
			(let ((env-binding (car env)))
				(if (eqv? (lhs env-binding) expr)
					; if we found a match, return the val
					(rhs env-binding)
					; otherwise recurse on the rest of the env
					(lookup expr (cdr env))
				)
			)
		)
	)

	(define (let? expr)
		(and
			(not (atom? expr))
			(eqv? (car expr) `let)
		)
	)

	(define (emit-let bindings body stack-index env)
		(let f ((b* bindings) (new-si stack-index) (new-env env))
			(if (null? b*)
				; if there are no bindings left, run the body
				(emit-expr body new-si new-env)
				; if there's another binding, bind it
				(let ((b (car b*)))
					(emit-expr (rhs b) new-si env) ; use the old env!
					(print "	movq %rax, " new-si "(%rsp)")
					(f
						(cdr b*)
						(- new-si WORD_SIZE)
						(extend-env (lhs b) new-si new-env)
					)
				)
			)
		)
	)

	(define bindings cadr)
	(define body caddr)
	(define lhs car)
	(define rhs cadr)

	(define (extend-env symbol stack-index env)
		(cons (list symbol stack-index) env)
	)

	(define (if? expr)
		(and
			(not (atom? expr))
			(eqv? (car expr) `if)
		)
	)

	(define (emit-if test conseq altern stack-index env)
		(let ((label-0 (unique-label)) (label-1 (unique-label)))
			(emit-expr test stack-index env) ; run test
			(print "	cmpq $" (immediate-rep #f) ", %rax") ; compare #f to %rax
			(print "	je " label-0) ; jump if %rax is false to the alternative
			(emit-expr conseq stack-index env) ; run consequent
			(print "	jmp " label-1); jump to end
			(emit-label label-0)
			(emit-expr altern stack-index env) ; run alternative
			(emit-label label-1) ; end
		)
	)

	(define label-counter 0)
	(define (unique-label)
		(let
			((new-label
				(string-append
					"scheme_label_"
					(number->string label-counter)
				)
			))
			(set! label-counter (+ label-counter 1))
			new-label
		)
	)

	(define (emit-label label) (print label ":"))

	(define test cadr)
	(define conseq caddr)
	(define altern cadddr)

	(define (emit-primcall x stack-index env)
		(define (emit-cmp bin-expr)
			(print "	cmpq $" bin-expr ", %rax")
			(print "	movq $0, %rax")
			(print "	sete %al")
			(print "	salq $" BOOL_SHIFT ", %rax")
			(print "	orq $" BOOL_TAG ", %rax")
		)

		(define (emit-type-check-primcall type-mask type-tag x env)
			(emit-expr (primcall-operand1 x) stack-index env)
			(print "	andq $" type-mask ", %rax")
			(emit-cmp type-tag)
		)

		(case (primcall-op x)
			; unary primcalls
			((add1)
				(emit-expr (primcall-operand1 x) stack-index env)
				(print "	addq $" (immediate-rep 1) ", %rax")
			)
			((sub1)
				(emit-expr (primcall-operand1 x) stack-index env)
				(print "	addq $" (immediate-rep -1) ", %rax")
			)
			((integer->char)
				(emit-expr (primcall-operand1 x) stack-index env)
				(print "	salq $" (- CHAR_SHIFT FIXNUM_SHIFT) ", %rax")
				(print "	orq $" CHAR_TAG ", %rax")
			)
			((char->integer)
				(emit-expr (primcall-operand1 x) stack-index env)
				(print "	sar $" (- CHAR_SHIFT FIXNUM_SHIFT) ", %rax")
			)

			((null?)
				(emit-expr (primcall-operand1 x) stack-index env)
				(emit-cmp EMPTY_LIST)
			)
			((integer?) (emit-type-check-primcall FIXNUM_MASK FIXNUM_TAG x env))
			((boolean?) (emit-type-check-primcall BOOL_MASK BOOL_TAG x env))
			((char?) (emit-type-check-primcall CHAR_MASK CHAR_TAG x env))
			((pair?) (emit-type-check-primcall PAIR_MASK PAIR_TAG x env))
			((vector?) (emit-type-check-primcall VECTOR_MASK VECTOR_TAG x env))
			((string?) (emit-type-check-primcall STRING_MASK STRING_TAG x env))
			((symbol?) (emit-type-check-primcall SYMBOL_MASK SYMBOL_TAG x env))

			((car)
				(emit-expr (primcall-operand1 x) stack-index env)
				(print "	movq -1(%rax), %rax") ; -1 because the of the pair tag
			)
			((cdr)
				(emit-expr (primcall-operand1 x) stack-index env)
				(print "	movq " (- WORD_SIZE 1) "(%rax), %rax")
			)

			; binary primcalls
			((+)
				; save arg 2 to the stack
				(emit-expr (primcall-operand2 x) stack-index env)
				(print "	movq %rax, " stack-index "(%rsp)")
				(emit-expr
					(primcall-operand1 x)
					(- stack-index WORD_SIZE)
					env
				)
				(print "	addq " stack-index "(%rsp), %rax")
			)
			((cons)
				(emit-expr (primcall-operand1 x) stack-index env)
				(print "	movq %rax, 0(%rsi)")
				(emit-expr (primcall-operand2 x) stack-index env)
				(print "	movq %rax, " WORD_SIZE "(%rsi)")
				(print "	movq %rsi, %rax")
				(print "	orq $" PAIR_TAG ", %rax")
				(print "	addq $" (* 2 WORD_SIZE) ", %rsi")
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
	(let ((func-name "_scheme_entry"))
		(print "	.text")
		(print "	.p2align 4")
		(print ".globl " func-name)
		(emit-label func-name)
	)

	; function body
	(emit-expr x (- WORD_SIZE) `())
	(print "	ret")
)

(compile-program
	`(cdr (cdr (cons 1 (cons 2 3))))
)
