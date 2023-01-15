; 4.

(defun elim(lista e)
    (cond
        ((null lista)
            nil
        )
        ((listp (car lista))
            (cons 
                (list (elim (car lista) e))
                (elim (cdr lista) e)
            )
        )
        ((equal (car lista) e)
            (elim (cdr lista) e)
        )
        (t
            (cons 
                (car lista)
                (elim (cdr lista) e)
            )
        )
    )
)

;(print (elim '(1 2 A (3 4 A) 5 A (A) 6) 'A))


; 5.

(defun verif(lista)
    (cond
        ((null lista)
            nil
        )
        ((AND (null (cdr lista)) (numberp (car lista)))
            nil
        )
        ((AND (null (cdr lista)) (listp (car lista)))
            t
        )
        ((AND (null (cdr lista)) (atom (car lista)))
            t
        )
        (t
            (verif (cdr lista))
        )
    )
)

(defun all_sublists(lista)
    (cond
        ((null lista) 0)
        ((atom lista) 0)
        ((verif lista)
            (+ 1
                (apply #'+ 
                    (mapcar #'all_sublists lista)
                )
            )
        )
        (t
            (apply #'+ 
                (mapcar #'all_sublists lista)
            )   
        )
    )
)

(print (all_sublists '(1 2 3 (5 A 2) B C (D C) (3 A) 4)))
(print (all_sublists '(1 (D C) (3) 4)))