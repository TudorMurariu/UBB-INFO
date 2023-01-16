; 2. Definiti o functie care obtine dintr-o lista data lista tuturor atomilor
; care apar, pe orice nivel, dar in aceeasi ordine. De exemplu
; (((A B) C) (D E)) --> (A B C D E)

; make_one_list(l1..ln) = { lambda(l1) + lambda(l2) + .. lambda(ln) }
; lambda(x) = {make_one_list(x), x - lista}
;           = {(x), altfel}

; fara functii map:
; (defun make_one_list(l)
;   (cond
;     ((null l) nil)
;     ((listp (car l)) (append (make_one_list (car l)) (make_one_list (cdr l))))
;     (t (append (list (car l)) (make_one_list (cdr l))))
;   )
; )


; cu functii map:
(defun make_one_list(l)
  (mapcan 
    #'(lambda (x) 
    (cond
      ((listp x) (make_one_list x))
      (t (list x))
    ) 
    )
  l
  )
)


(print (make_one_list (list 1 2 3 4 6)))
(print (make_one_list (list (list (list 'A 'B) 'C) (list 'D 'E))))
(print (make_one_list '(1 (1 2) 3 (A B))))