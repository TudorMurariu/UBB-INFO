/*
    8. Sa se sorteze o lista cu eleiminarea dublurilor.

    plaseaza(l1..ln, X) = { [X] , n = 0 }
                          { plaseaza(l2..ln, X) , X > l1 }
                          { X + l1..ln , X < l1 }
                          { l1..ln , X = l1 }

    plaseaza(L, X, R)
    L : lista data                     ( lista de numere intregi )
    X : elementul care trebuie plasat  ( numar intreg )
    R : lista raspuns                  ( lista de numere intregi )

    Modele de flux : (i,i,o) (i,i,i)
*/

plaseaza([], X, [X]):- !.

plaseaza([H|T], X, [H|R]):- X > H, !,
                        plaseaza(T, X, R).

plaseaza([H|T], X, [X,H|T]):- X < H, !.

plaseaza([H|T], X, [H|T]):- X = H, !.

/*
    sorteaza(l1..ln) = { [] , n = 0 }
                       { plaseaza(sorteaza(l2..ln), l1) , altfel }

    sorteaza(L, R)
    L : lista data      ( lista de numere intregi )
    R : lista sortata   ( lista de numere intregi )

    Modele de flux : (i,o) (i,i) 

*/

sorteaza([], []):- !.

sorteaza([H|T], R):- sorteaza(T, R1),
                     plaseaza(R1, H, R).