/*  a. Sa se scrie un predicat care intoarce diferenta a doua multimi. */


/*
    apartine_lista(X, l1..ln) = { false, n = 0 }
                                { false, X == l1 }
                                { apartine_lista(X, l2..ln), altfel }

    apartine_lista(X, L)
    X : numar intreg dat
    L : lista de numere intregi data

    Modele de flux :  (i,i)
*/

apartine_lista(_, []):- false, !.
apartine_lista(X, [X|_]):- !.
apartine_lista(X, [_|T]):- apartine_lista(X, T).


/*
    dif_multimi(l1..ln, d1..dm) = { [], n = 0 }
                            {  dif_multimi(l2..ln, d1..dm), apartine_lista(l1, d1..dn)}
                            {  l1 + dif_multimi(l2..ln, d1..dm), altfel }
    
    dif_multimi(L1, L2, R)
    L1 : prima multime
    L2 : a 2-a multime
    R : lista raspuns

    Modele de flux : (i,i,o)  (i,i,i)
*/

dif_multimi([], _, []):- !.
dif_multimi([H|T], L2, R):- apartine_lista(H, L2), !,
                            dif_multimi(T, L2, R).

dif_multimi([H|T], L2, R):- dif_multimi(T, L2, R1),
                            R = [H|R1].