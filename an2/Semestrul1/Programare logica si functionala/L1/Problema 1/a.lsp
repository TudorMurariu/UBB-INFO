;; a) Sa se insereze intr-o lista liniara un atom a dat dupa al 2-lea, al 4-lea,
;; al 6-lea,....element.

(defun inserare(lista el index)
    (cond
        ((null lista) nil)
        ((eq (mod index 2) 0) 
            (cons
                (car lista)
                (cons 
                    el 
                    (inserare (cdr lista) el (+ index 1))
                )
            )
        )
        (t 
            (cons
                (car lista)
                (inserare (cdr lista) el (+ index 1))
            )
        )
    )
)

(print (inserare '(1 2 3 4 5 6 7 8) 68 1))