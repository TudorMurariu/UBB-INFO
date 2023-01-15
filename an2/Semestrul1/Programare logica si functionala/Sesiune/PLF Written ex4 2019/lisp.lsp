; 1.
; folosim o functie lambda

(defun F(L)
    ((lambda (X)
        (cond
            ((atom L) -1)
            ((> X 0) (+ (CAR L) X (F (CDR L))))
            (t (F (CDR L)))
        )
     )
     (F (CAR L))
    )
)

; 4.

(defun inlocuire(L nivel)
    (cond
        ((null L) nil)
        ((AND (= nivel 1) (atom (car L)))
            (append
                (list 0)
                (inlocuire (cdr L) nivel)
            )
        )
        ((= nivel 1) ;if list
            (append
                (list (car L))
                (inlocuire (cdr L) nivel)
            )
        )
        ((listp (car L))
            (append
                (list (inlocuire (car L) (- nivel 1)))
                (inlocuire (cdr L) nivel)
            )
        )
        (t
            (append
                (list (car L))
                (inlocuire (cdr L) nivel)
            )
        )
    )
)

(print (inlocuire '(F (1 R (E E B) I) R (D Y E (E))) 2 ))


; 5.

(defun minim_impare(L &optional (nivel 1))
    (cond
        ((null L) 999998)
        ((AND (atom L) (eq (MOD nivel 2) 0))
            999998
        )
        ((numberp L) ;si nivel impar
            L
        )
        ((atom L)
            999998
        )
        ((listp (car L))
            (min 
                (minim_impare (car L) (+ nivel 1))
                (minim_impare (cdr L) nivel)
            )
        )
        ((eq (MOD nivel 2) 0) ; atom
            (minim_impare (car L) nivel)
        )
        ((numberp (car L)) ; nivel impar
            (min 
                (car L)
                (minim_impare (cdr L) nivel)
            )
        )
        (T ;atom nenumeric
            (minim_impare (cdr L) nivel)
        )
    )
)

(defun nr(L)
    (cond
        ((NULL L) 0)
        ((atom L) 0)
        ((eq (MOD (minim_impare L) 2) 0)
            (+ 1 
                (apply #'+ 
                    (mapcar #'nr L)
                )
            )
        )
        (T
            (apply #'+ 
                (mapcar #'nr L)
            )
        )
    )
)

(print (nr  '(A (B 2) (2 C 4) (1 (3 F)) (((G) 4) 2))  ))