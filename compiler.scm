(define (compile-program x)
	; function header
	(print "	.text")
	(print "	.p2align 4")
	(print ".globl _scheme_entry")
	(print "_scheme_entry:")

	; function body
	(print "	movl $" x ", %eax")
	(print "	ret")
)

(compile-program 4)
