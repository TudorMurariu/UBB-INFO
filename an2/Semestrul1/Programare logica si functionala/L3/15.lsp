; Sa se construiasca o functie care intoarce numarul atomilor dintr-o
; lista, de la orice nivel.

(defun nr_atomi(lista)
    (cond 
        ((null lista) 0)
        ((atom lista) 1)
        (t
            (apply #'+
                (mapcar #'nr_atomi lista)
            )
        )
    )
)

(print (nr_atomi '(1 2 32)))
(print (nr_atomi '(1 2 (1 2) A)))
(print (nr_atomi '( (1) ((1)) )))
(print (nr_atomi '( F R E E (B I R (D) Y) E (E E E) A )))