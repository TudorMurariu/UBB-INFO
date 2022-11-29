# Seminarul 5 - Recursivitate in Lisp

## - P1 Se dau doua liste liniare, numerice, formate din elemente distincte sortate crescator. Se cere sa se interclaseze listele date.
**Ex:**
L1 = [1,3,5,9]    } <br>
L2 = [2,3,4,6,8]  }   => [1,2,3,4,5,6,8,9] <br>


**Model Matematic**
interclasare(l1..ln, h1..hm) = { h1..hm , n = 0 } <br>
                               { l1..ln , m = 0 si n != 0 } <br>
                    { l1 + interclasare(l2..ln,h1..hm) ,l1 < h1 si n != 0 si m != 0 } <br>
                    { l1 + interclasare(l2..ln,h2..hm) ,l1 == h1 si n != 0 si m != 0 } <br>
                    { h1 + interclasare(l1..ln,h2..hm) ,l1 > h1 si n != 0 si m != 0 } <br>

**Implementare**

---lisp
(defun interclasare (l,k)
    (cond
        ( (AND (null l) (not (null k)) k) )
        ( (null k) l )
        ( (< (car l) (car k)) (cons(car l) (interclasare(cdr l) k) ) )
        ( (< (car l) (car k)) (cons(car k) (interclasare l (cdr k)) ) )
        ( t (cons(car l) (interclasare (cdr l) (cdr k)) ) )
    ) 
)

---
