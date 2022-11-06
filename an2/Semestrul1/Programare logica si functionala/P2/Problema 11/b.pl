/*
dubleaza_prim_liste(l1,l2..ln) = { [], n = 0
                                 { dubleaza_prim(l1) + dubleaza_prim_liste(l2..ln), n != 0 si is_list(l1)
                                 { l1 + dubleaza_prim_liste(l2..ln), n != 0 si number(l1)

dubleaza_prim_liste(L, R)
       L - lista data de numere intregi si liste
       R - lista raspuns

       Metode de flux : (i, o) , (i, i)
*/


dubleaza_prim_liste([], []):- !.

dubleaza_prim_liste([H|T], R):- is_list(H), !,
                                dubleaza_prim(H, R1),
                                dubleaza_prim_liste(T, R2),
                                R = [R1|R2].

dubleaza_prim_liste([H|T], R):- number(H), !,
                                dubleaza_prim_liste(T, R2),
                                R = [H|R2].
