; 10. Se da un arbore de tipul (2). Sa se precizeze nivelul pe care apare un nod
; x in arbore. Nivelul radacii se considera a fi 0.

; (A (B) (C (D) (E)))

(defun nivel(l x depth)
    (cond
        ((null l) -1)
        ((eq x (car l)) depth)
        ((null (cdr l)) -1)
        ((null (cddr l)) (print "A" ) (nivel (cadr l) x (+ depth 1)))
        (t (MAX (nivel (cadr l) x (+ depth 1)) (nivel (caddr l) x (+ depth 1))))
    )
)

(print (nivel (list 'A (list 'B) (list 'C (list 'D) (list 'E))) 'E 0))