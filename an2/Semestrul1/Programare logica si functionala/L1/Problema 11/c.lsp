;c) Sa se elimine toate aparitiile elementului numeric maxim dintr-o lista
; neliniara.

; max(l1..ln) = { max , n = 0 }
;               { max , max(l2..ln) <= max }
;               { l1 , max(l2..ln) > max }

(defun max (l, maxi) 
    (cond
        ((null l) maxi)
        ((< (car l) maxi) (max (cdr l) (car l)))
        (t (max (cdr l) (car l)))
    )
)

; elim_max(l1..ln) = { [] , n = 0 }
;

(defun elim_e (l, e)
    (cond
        ((null l) nil)

    )
)

;
;

(defun main (l)
    
)