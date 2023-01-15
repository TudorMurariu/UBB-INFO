;1
;; Folosim lambda:

(defun f(L)
    ((lambda X
        (cond
            ((NULL L) 0)
            ((> X 2) (+ (CAR L) (F (CDR L))))
            (t X)
        )
     )
     (F (CAR L))
    )
)