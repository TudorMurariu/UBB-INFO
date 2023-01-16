; Sa se elimine dintr-o lista toate elementele care se repeta (de la orice nivel)

; exista(L E)

; elimina(L E)

; elimina_dub(L)


; exista(l1..ln E) = {nil , n = 0}
;                  = {t , l1 = E}
;                  = {exista(l1, E) sau exista(l2..ln, E), l1 - lista}
;                  = {exista(l2..ln, E), altfel}
;

(defun exista(L E)
    (cond
        ((null L) nil)
        ((eq (car L) E)
            t
        )
        ((listp (car L))
            (OR 
                (exista (car L) E)
                (exista (cdr L) E)
            )
        )
        (t
            (exista (cdr L) E)
        )
    )
)

;(print (exista '(1 2 2 A (5 6 (3)) 1) 3))
;
; elimina(l1..ln, E) = {nil , n = 0} 
;                    = {elimina(l2..ln, E), l1 = E}
;                    = {(elimina l1 E), l1 - lista}
;                    = {l1 + elimina(l2..ln, E), altfel}
;

(defun elimina(L E)
    (cond
        ((null L) nil)
        ((eq (car L) E)
           (elimina (cdr L) E) 
        )
        ((listp (car L))
            (append
                (list (elimina (car L) E))
                (elimina (cdr L) E)
            )
        )
        (t
            (append
                (list (car L))
                (elimina (cdr L) E)
            )
        )
    )
)

;(print (elimina '(1 2 (1 2) 3) 1))
;(print (elimina '(1 2 2 A (2 6 (2)) 1) 2))
; elimina_dub(l1..ln) = {nil , n = 0 }
;                     = {elimina_dub(l1) + elimina_dub(l2..ln), l1 - lista }
;                     = {elimina_dub(elimina(l1..ln, l1)), exista(l2..ln, l1) }
;                     = {l1 + elimina_dub(l2..ln), altfel }
;

(defun elimina_dub(L)
    (cond
        ((null L) nil)
        ((listp (car L))
            (append
                (list (elimina_dub (car L)))
                (elimina_dub (cdr L))
            )
        )
        ((exista (cdr L) (car L))
            (elimina_dub
                (elimina L (car L))
            )
        )
        (t
            (append
                (list (car L))
                (elimina_dub (cdr L))
            )
        )
    )
)

;(print (elimina_dub '(1 2 (1 3)) ))
;(print (elimina_dub '(1 2 2 A B (2 6 (2 3) C) 1 A) ))

(print (elimina_dub '(1 (2 C F 1 (D 2 C 4)) E)))
(print (elimina_dub '(1 2 3 4 5 4 3 2 1) ))
(print (elimina_dub '(A (B C) 3 A (D C A) 5 5) ))
(print (elimina_dub '(1 2 (A B A (3 A)) (1 (1 (C)) 1))   ))