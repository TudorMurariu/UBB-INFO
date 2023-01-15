; b) Definiti o functie care obtine dintr-o lista data lista tuturor atomilor
; care apar, pe orice nivel, dar in ordine inversa. De exemplu: (((A B) C)
; (D E)) --> (E D C B A)

(defun invers_all_levels(lista)
    (print lista)
    (cond
        ((AND (null (cdr lista)) (listp (car lista)))
            (invers_all_levels (car lista))
        )
        ((null (cdr lista))
            (list (car lista))
        )

        ((listp (car lista))
            (append 
                (invers_all_levels (cdr lista))
                (invers_all_levels (car lista))
            )
        )
        (t 
            (append 
                (invers_all_levels (cdr lista))
                (list (car lista))
            )
        )
    )
)

(print (invers_all_levels '(A B C (D E) F)))


