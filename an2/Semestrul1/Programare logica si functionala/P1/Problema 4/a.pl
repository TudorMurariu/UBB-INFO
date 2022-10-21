/*
substituire(l1..ln,e1,e2){ [], n = 0 }
                         { [l1 + substituire(l2..ln,e1,e2)], e1 != l1}
                         { [e2 + substituire(l2..ln,e1,e2)] , e1 = l1 }

Predicat: substituire(L, E1, E2, R)
L - o lista de numere intregi
E1 - valoarea elementelor care trebuie substituite
E2 - valoare cu care vom substitui
R - lista raspuns formata din lista initiala cu elementele egale cu E1 sunstituite cu E2
*/


substituire([], _, _, R):- R = [].

substituire([H|T], E1, E2, R):- E1 \= H,
                                substituire(T, E1, E2, R1),
                                R = [H|R1].

substituire([H|T], E1, E2, R):- E1 = H,
                                substituire(T, E1, E2, R1),
                                R = [E2|R1].