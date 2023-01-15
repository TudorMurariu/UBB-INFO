; 1.
;; Putem inlocui cu:

(defun F(L1 L2)
    ((LAMBDA (X)
        (APPEND X
            (COND 
                ((NULL L1) (CDR L2))
                (t (list X (car L2)))
            )
        )
     )
     (F (CAR L1) L2)
    )
)
