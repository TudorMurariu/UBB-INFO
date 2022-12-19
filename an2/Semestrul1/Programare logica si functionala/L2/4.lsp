;; transform(l1..ln) = { [] , n == 0 }
;;                     { [l1, 0] , n == 1 }
;;                     { [l1, 1] + transform(l2), n == 2 }
;;                     { [l1, 2] + transform(l2) + transform(l3) , altfel (n == 3)}

;; parcurgem in preordine
(defun transform(l)
    (cond 
        ((null l) nil)
        ((null (cadr l)) (append (list (car l)) (list 0) ))
        ((null (caddr l)) (append (list (car l)) (list 1) (transform (cadr l))))
        (t (append (list (car l)) (list 2) (transform (cadr l)) (transform (caddr l))))
    )
)

(print (transform '(A (B) (C (D) (E)))))
(print (transform '(A (B (D) (E (F (G) (H)))) (C (I (J (K))))))) 
(print (transform '(A (B (H (I (J) (K)))) (C (D (G)) (E)))))
(print ((lambda (x) (* x x)) 2))
