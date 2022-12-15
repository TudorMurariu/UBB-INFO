;; transform(l1..ln) = { [] , n = 0 }


(defun transform(l)
    (cond 
        ((null l) nil)
        ((null (cadr l)) (append (list (car l)) (list 0) ))
        ((null (caddr l)) (append (list (car l)) (list 1) (transform (cadr l))))
        (t (append (list (car l)) (list 2) (transform (cadr l)) (transform (caddr l))))
    )
)

(print (transform '(A (B) (C (D) (E)))))
(print (transform '(A(B(D)(E(F(G)(H))))(C(I(J(K)))))))