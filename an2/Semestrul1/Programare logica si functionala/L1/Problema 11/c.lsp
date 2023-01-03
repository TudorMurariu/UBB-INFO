;c Sa se elimine toate aparitiile elementului numeric maxim dintr-o lista
; neliniara.

; maxim(l1..ln, maxi) = { maxim , n = 0 }
;               { maxim(l2..ln, maxim(l1, maxi)) , l1 - lista }
;               { maxi , maxim(l2..ln) <= maxi }
;               { l1 , maxim(l2..ln) > maxi }

(defun maxim (l maxi) 
    (cond
        ((null l) maxi)
        ((listp (car l)) (maxim (cdr l) (maxim (car l) maxi)))
        ((AND (numberp (car l)) (> (car l) maxi)) (maxim (cdr l) (car l)))
        (t (maxim (cdr l) maxi))
    )
)


; elimina(l1..ln, e) = { [], n = 0 }
;                    { elimina(l2..ln, e) , l1 = e }
;                    { l1 + elimina(l2..ln, e) , l1 != e si l1 atom }
;                    { elimina(l1, e) + elimina(l2..ln, e) , altfel}
;

(defun elimina(l e)
    (cond 
        ( (null l) nil )
        ( (equal (car l) e) (elimina (cdr l) e) )
        ( (atom (car l)) (cons (car l) (elimina (cdr l) e)) )
        ( t (cons (elimina (car l) e) (elimina (cdr l) e)) )
    )
)

; main(l1..ln) = { elimina(l, maxim(l, -9999)) }

(defun main (l)
    (elimina l (maxim l -9999))
)

;(write (elimina (list 1 69 "ana" (list 5 5 3) 1 -2 -9999 5 6 7 5 8) 5))

(write (main (list 1 33 "ana" (list 5 5 3) 1 -2 -9999 5 6 7 33 5 8 33)))
(terpri) ; endline