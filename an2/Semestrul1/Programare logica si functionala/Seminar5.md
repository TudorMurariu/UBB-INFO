# **Seminarul 5** - Recursivitate in Lisp

## **P1** Se dau doua liste liniare, numerice, formate din elemente distincte sortate crescator. Se cere sa se interclaseze listele date.
**Ex:** <br>
```
L1 = [1,3,5,9]    }
L2 = [2,3,4,6,8]  }   => [1,2,3,4,5,6,8,9]
```

**Model Matematic**
```python
interclasare(l1..ln, h1..hm) = { h1..hm , n = 0 } 
                               { l1..ln , m = 0 si n != 0 } 
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

| defun          | cons         | list          | append    |
|:--------------:|:------------:|:-------------:|:---------:|
| 'A 'B          | cons('A 'B)  | (A B)         | Eroare    |
| 'A '(B C)      | (A B C)      | (A (B C))     | Eroare    |
| '(A B)         | ((AB).C)     | (A B).C)      | (A B.C)   |
| '(A B) '(C D)  | ((A B) C D)  | ((A B) (C D)) | (A B C D) |
| 'A 'B 'C       | Eroare       | (A B C)       | Eroare    |
| '(A) '(B) 'C   | Eroare       | (A B C)       | (A B.C)   |

## **P2** Sa se elimine toate aparitiile unui aton dintr-o lista.

**Ex:** <br>
```
'(2 (A (1 3 (1)) 4 1) 5 D 1) 1 => (2 (A (3 ()) 4) 5 D)
```

**Model Matematic**
```python
elimina(l1..ln, e) { [], n = 0 }
                   { elimina(l2..ln, e) , l1 = e }
                   { l1 + elimina(l2..ln, e) , l1 != e si l1 atom }
                   { elimina(l1, e) + elimina(l2..ln, e) , altfel}

eq 
equal
number
listp
oddp
atom
``` 

**Implementare**
```lisp
(defun elimina(l e)
    (cond 
        ( (null l) nil )
        ( (equal (car l) e) (elimina (cdr l) e) )
        ( (atom (car l)) (cons (car l) (elimina (crd l) e)) )
        ( t (cons (elimina (car l) e)) (elimina (cdr l) e) )
    )
)
```

## **P3** 

**Ex:** <br>
```

```
