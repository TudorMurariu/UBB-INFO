;14. Sa se construiasca lista nodurilor unui arbore de tipul (2) parcurs in
; postordine.


(defun postordine(root)
    (cond
        ((null root) nil)
        ((null (cdr root))
            (list (car root))
        )
        ((null (cddr root))
            (append
                (postordine (cadr root))
                (list (car root))
            )
        )
        (T
            (append
                (postordine (cadr root))
                (postordine (caddr root))
                (list (car root))
            )
        )
    )
)

(print (postordine '(A (B) (C (D) (E))) ))