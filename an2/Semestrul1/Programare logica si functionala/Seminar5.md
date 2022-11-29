# **Seminarul 5** - Recursivitate in Lisp

## **P1** Se dau doua liste liniare, numerice, formate din elemente distincte sortate crescator. Se cere sa se interclaseze listele date.
**Ex:** <br>
L1 = [1,3,5,9]    } <br>
L2 = [2,3,4,6,8]  }   => [1,2,3,4,5,6,8,9] <br>


**Model Matematic**
```ruby
interclasare(l1..ln, h1..hm) = { h1..hm , n = 0 } <br>
                               { l1..ln , m = 0 si n != 0 } <br>
                               { l1 + interclasare(l2..ln,h1..hm) ,l1 < h1 si n != 0 si m != 0 }
                               { l1 + interclasare(l2..ln,h2..hm) ,l1 == h1 si n != 0 si m != 0 }
                               { h1 + interclasare(l1..ln,h2..hm) ,l1 > h1 si n != 0 si m != 0 }
```
**Implementare**
```lisp
;;; (car l) -> l1 (primul element din lista)
;;;  (cdr l) -> l2..ln (lista fara primul element)

(defun interclasare (l,k)
    (cond
        ( (AND (null l) (not (null k)) k) )
        ( (null k) l )
        ( (< (car l) (car k)) (cons(car l) (interclasare(cdr l) k) ) )
        ( (< (car l) (car k)) (cons(car k) (interclasare l (cdr k)) ) )
        ( t (cons(car l) (interclasare (cdr l) (cdr k)) ) )
    ) 
)
```

| defun          | cons              | list          | append    |
|----------------|-------------------|---------------|-----------|
| 'A 'B          | cons('A 'B) (A.B) | (A B)         | Eroare    |
| 'A '(B C)      | (A B C)           | (A (B C))     | Eroare    |
| '(A B)         | ((AB).C)          | ((A B).C)     | (A B.C)   |
| '(A B) '(C D)  | ((A B) C D)       | ((A B) (C D)) | (A B C D) |
|   |   |   |   |
