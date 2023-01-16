; 3.

(DEFUN F(L)
    ((lambda (X)
        (cond
            ((null L) 0)
            ((> X 2) (+ (car L) (F (cdr L))))
            (t (F (CAR L)))
        )
     )
     (F (CAR L))
    )
)


;4.

(defun cale(Root Nod)
    (cond
        ((null Root) nil)
        ((eq (car Root) Nod)
            (list Nod)
        )
        ((null (cdr Root)) nil)
        ((null (cddr Root)) 
            (append
                (list (car Root))
                (cale (cadr Root) Nod)
            )
        )
        (t
            (if (cale (cadr Root) Nod)
                (append
                    (list (car Root))
                    (cale (cadr Root) Nod)
                )
                (append
                    (list (car Root))
                    (cale (caddr Root) Nod)
                )
            )
        )
    )
)

;(print (cale '(A (B) (C (D) (E))) 'E))


;5.

(defun verif(L)
    (cond
        ((NULL L) nil)
        ((AND (numberp (car L)) (= (MOD (car L) 2) 1))
            t
        )
        ((numberp (car L))
            nil
        )
        (T
            (verif (cdr L))
        )
    )
)

(defun nr_sublist(L)
    (cond
        ((null L) 0)
        ((atom L) 0)
        ((verif L)
            (+ 1 
                (apply #'+
                    (mapcar #'nr_sublist L)
                )
            )
        )
        (t
            (apply #'+
                (mapcar #'nr_sublist L)
            )
        )
    )
)

(print (nr_sublist '(A (B 2) (1 C 4) (D 1 (5 F)) ((G 4) 6)) ))