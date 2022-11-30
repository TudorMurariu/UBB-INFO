;;;; cmmmdc(a, b) = { b , a = 0 }
;;;;              = { a , b = 0 }
;;;;              = { a , a = b }
;;;;              = { cmmmdc(a % b, b) , a > b }
;;;;              = { cmmmdc(a,b) , altfel }
;;;;

(defun cmmdc (a b)
  (cond
    ((= a 0) b)
    ((= b 0) a)
    ((= a b) a)
    ((> a b) (cmmdc (mod a b) b))
    (t (cmmdc b a))
  )
)

;; cmmmc(a, b) = a*b / cmmdc(a, b)

(defun cmmmc (a b)
    (cond
        ((= a 0) b)
        ((= b 0) a)
        (t (floor (* a b) (cmmdc b a)))
    )
)


;; cmmdc_list(l1..ln, E) = { E , n = 0 }
;;                         { cmmdc_list(l2..ln, cmmdc(l1, E)) , number(l1) }
;;                         { cmmdc_list(l2..ln, E) , altfel }

(defun cmmmc_list (l e)
    (cond
        ((null l) e)
        ((numberp (car l)) (cmmmc_list (cdr l) (cmmmc e (car l))))
        (t (cmmmc_list (cdr l) e))
    )
)

; (defun afis (l)
;   (cond
;     ((null l) nil)
;     (t (write (car l)) (afis (cdr l)))
;   )
; )

(write (cmmmc_list (list 2 2 (list 1 4 5) "ana" "pufi" 5) 1))