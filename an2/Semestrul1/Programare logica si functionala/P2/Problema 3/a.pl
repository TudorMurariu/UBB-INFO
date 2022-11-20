/*

    sorteaza(L, R)
    L : lista data
    R : lista sortata dupa cerinta

    Modele de flux : (i,o) (i,i)

    plaseaza(L, X, R)
    L : lista data
    X : elementul care trebuie adaugat
    R : lista raspuns

    Modele de flux : (i,i,o) (i,i,i)

*/

plaseaza([], X, [X]):- !.

plaseaza([H|T], X, [H|R1]):- X > H, !,
                             plaseaza(T, X, R1).

plaseaza([H|T], X, [X,H|T]):- X < H, !.    

%altfet
plaseaza(L, _, L).

sorteaza([], []).
sorteaza([H|T], R):- sorteaza(T, R1),
                     plaseaza(R1, H, R).
                     