; d) Sa se construiasca o functie care intoarce produsul atomilor numerici pari
; dintr-o lista, de la orice nivel.

; produs(l1..ln, p) = { p , n = 0 }
;                     { 0 , l1 = 0 si l1 - number }
;                     { l1 * produs(l1..ln, p) , l1 - number}
;                     { produs(l1..ln, p) , altfel }

(defun produs (l p)
 (cond
    ((null l) p)
    ((AND (numberp (car l)) (eq (car l) 0)) 0)
    ((AND (numberp (car l)) (eq (MOD (car l) 2) 0)) (* (car l) (produs (cdr l) p)))
    ((listp (car l)) (produs (cdr l) (produs (car l) p)))
    (t (produs (cdr l) p))
 )
)

(defun main_produs (l)
   (produs l 1)
)

(write (main_produs (list 1 2 3 5 "ana" (list 0 1))))