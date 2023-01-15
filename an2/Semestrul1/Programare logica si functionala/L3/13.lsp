; 13. Definiti o functie care substituie un element prin altul la toate
; nivelurile unei liste date.

(defun subst_list(L E R)
    (cond
        ((atom L) (if (equal E L) R L))
        (t ;L - lista
            (mapcar 
                #'(lambda (lista)
                    (subst_list lista E R)
                )
                L
            )
        )
    )
)

(print (subst_list '(A B (A B) A (B (A B)) C D A D) 'A 3))
(print (mapcan #'subst_list '(A B (A B) A (B (A B)) C D A D) 'A 3))