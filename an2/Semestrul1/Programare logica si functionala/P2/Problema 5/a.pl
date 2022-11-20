/*

    poz_max(L, R)
    L : lista data
    R : lista raspuns

    poz_max(L, Count, Max, Raux, R)

*/

poz_max([], []):- !.

poz_max(L, R):- poz_max(L, 1, 0, [], R).

poz_max([], _, _, Raux, R):- reverse(Raux, R), !.

poz_max([H|T], Count, Max, _, R):- H > Max, !,
                                      Count1 is Count + 1,
                                      poz_max(T, Count1, H, [Count], R).
                                   
poz_max([H|T], Count, Max, Raux, R):- H = Max, !,
                                      Count1 is Count + 1,
                                      poz_max(T, Count1, Max, [Count|Raux], R).

poz_max([H|T], Count, Max, Raux, R):- H < Max, !,
                                      Count1 is Count + 1,
                                      poz_max(T, Count1, Max, Raux, R).
