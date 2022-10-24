/*
substituire(l1..ln,e1,L1..n){ [], n = 0 }
                         { l1 + substituire(l2..ln,e1,e2), e1 != l1}
                         { L + substituire(l2..ln,e1,e2) , e1 = l1 }



Predicat: substituire(L, E1, L2, R)
L - o lista de numere intregi
E1 - valoarea elementelor care trebuie substituite
L2 - lista cu care vom substitui valorile lui E1
R - lista raspuns formata din lista initiala cu elementele egale cu E1 sunstituite cu lista L2
Modele de flux: (i, i, i, o) , (i, i, i, i) 

Teste:
substituire([], 3, 4, R).
R = []
substituire([1, 2, 1, 2, 1, 3], 1, [99], R).
R = [ [99], 2, [99], 2, [99], 3]
substituire([5, 5, 5], 5, [6,2], R).
R = [ [6,2], [6,2], [6,2]]
substituire([5, 5, 5], 1, 6, R).
R = [5, 5, 5]
*/


substituire([], _, _, R):- R = [].

substituire([H|T], E1, L2, R):- E1 \= H,
                                substituire(T, E1, L2, R1),
                                R = [H|R1].

substituire([H|T], E1, L2, R):- E1 = H,
                                substituire(T, E1, L2, R1),
                                R = [L2|R1].