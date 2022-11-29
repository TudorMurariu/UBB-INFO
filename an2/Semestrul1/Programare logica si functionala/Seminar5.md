# Seminarul 5 - Recursivitate in Lisp

## P1 Se dau doua liste liniare, numerice, formate din elemente distincte sortate crescator. Se cere sa se interclaseze listele date.
**Ex:**
L1 = [1,3,5,9]    }
L2 = [2,3,4,6,8]  }   => [1,2,3,4,5,6,8,9]


**Model Matematic**
interclasare(l1..ln, h1..hm) = { h1..hm , n = 0 }
                               { l1..ln , m = 0 }
                               {  }