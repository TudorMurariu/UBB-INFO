; Sa se scrie o functie care sa testeze daca o lista liniara formata din
;numere intregi are aspect de "munte"(o secvență se spune ca are aspect de
;"munte" daca elementele cresc pana la un moment dat, apoi descresc. De
;ex. 10 18 29 17 11 10)

;; munte(l1..ln) = { false , n = 0 }
;;                 { true ,  }

(defun munte (l aux)
    (cond
        ((null l) nil)
        ((null (cdr l)) t)
        ((> (car l) (car (cdr l))) nil)
        (t (munte (cdr l)))
    )
)

(write (munte (list 2 2) 1))