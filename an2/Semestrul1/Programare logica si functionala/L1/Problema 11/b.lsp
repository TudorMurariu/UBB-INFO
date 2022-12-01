; Sa se scrie o functie care sa testeze daca o lista liniara formata din
;numere intregi are aspect de "munte"(o secvență se spune ca are aspect de
;"munte" daca elementele cresc pana la un moment dat, apoi descresc. De
;ex. 10 18 29 17 11 10)

;; d = 0 deal , d = 1 vale
;; munte(l1..ln, d) = { false , n = 0 }
;;                    { false , n = 1 si d = 0 }
;;                    { true , n = 1 si d = 1 }
;;                    { false , d = 1 si l1 < l2 }
;;                    { munte(l2..ln, 0), d = 0 si l1 < l2 }
;;                    { munte(l2..ln, 1), l1 > l2 }

(defun munte (l d)
    (cond
        ((null l) nil)
        ((AND (null (cdr l)) (eq d 0)) nil)
        ((AND (null (cdr l)) (eq d 1)) t)
        ((= (car l) (car (cdr l))) nil)
        ((AND (= d 1) (< (car l) (car (cdr l)))) nil)
        ((AND (= d 0) (< (car l) (car (cdr l)))) (munte (cdr l) 0))
        (t (munte (cdr l) 1)) ;((> (car l) (car (cdr l))))
    )
)

(defun munte_main (l)
    (cond
        ((null l) nil)
        ((null (cdr l)) nil)
        ((>= (car l) (car (cdr l))) nil)
        (t (munte l 0))
    )
)

(write (munte_main (list 2 1)))