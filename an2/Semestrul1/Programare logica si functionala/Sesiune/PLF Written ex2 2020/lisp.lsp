;1. Folosim lambda:

(defun Fct(F L)
    ((lambda (X)
        (cond
            ((NULL L) nil)
            (X 
                (cons X (Fct F (CDR L)))
            )
            (T nil)
        )
     )

    )
)