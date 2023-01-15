; c) Definiti o functie care intoarce cel mai mare divizor comun al numerelor
; dintr-o lista neliniara.

(defun cmmdc(a b)
    (cond
        ((eq a 0) b)
        ((eq a b) a)
        (t (cmmdc (MOD b a) a))
    )
)

(defun cmmdcLista(lista d)
    (cond
        ((null lista) d)
        ((numberp (car lista))
            (cmmdcLista (cdr lista) (cmmdc d (car lista)))
        )
        ((listp (car lista))
            (cmmdcLista 
                (cdr lista) 
                (cmmdcLista (car lista) d)
            )
        )
        (t
            (cmmdcLista (cdr lista) d)
        )
    )
)

(defun main(lista)
    (cmmdcLista lista (car lista))
)

(print (main '(64 24 (16 72 (8) 16 (A M O NGUS)))))