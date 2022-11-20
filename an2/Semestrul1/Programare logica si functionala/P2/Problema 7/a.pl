/*

    inmultire(L, X, R, Rest)
    L : lista
    X : inmultire
    R : lista raspuns

*/

inmultire([], _ , []):- !.

inmultire(L, X, R):- inmultire(L, X, [H|R], 0),
                     H = 0, !.


inmultire(L, X, R):- inmultire(L, X, R, 0).

inmultire([E], X, [Cat], Rest):- !,
                                 Cat is (E * X) % 10,
                                 Rest is (E * X) // 10.

inmultire([H|T], X, R, Rest):- inmultire(T, X, R1, Rest1),
                               Cat is (H * X + Rest1) % 10,
                               Rest is (H * X + Rest1) // 10,
                               R = [Cat|R1].



