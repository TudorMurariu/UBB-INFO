/*
substituire(l1..ln,e1,e2){ [], n = 0 }
                         { [l1 + substituire(l2..ln,e1,e2)], e1 != l1}
                         { [e2 + substituire(l2..ln,e1,e2)] , e1 = l1 }



Predicat: substituire(L, E1, E2, R)
L - o lista de numere intregi
E1 - valoarea elementelor care trebuie substituite
E2 - valoare cu care vom substitui
R - lista raspuns formata din lista initiala cu elementele egale cu E1 sunstituite cu E2
Modele de flux: (i, i, i, o) , (i, i, i, i) 

Teste:
substituire([],3,4,R).
R = []
substituire([1, 2, 1, 2, 1, 3],1,99,R).
R = [99, 2, 99, 2, 99, 3]
substituire([5, 5, 5],5,6,R).
R = [6, 6, 6]
substituire([5, 5, 5],1,6,R).
R = [5, 5, 5]
*/


substituire([], _, _, R):- R = [].

substituire([H|T], E1, E2, R):- E1 \= H,
                                substituire(T, E1, E2, R1),
                                R = [H|R1].

substituire([H|T], E1, E2, R):- E1 = H,
                                substituire(T, E1, E2, R1),
                                R = [E2|R1].