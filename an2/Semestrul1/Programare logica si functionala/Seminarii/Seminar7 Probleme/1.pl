/* Se consideră problema ștergerii dintr-o listă a tuturor elementele care se repetă.
De ex.,pentru lista [1,2,3,2,1] rezultatul așteptat este lista [3]. */

/*
    exista(L, E) -> true/false
*/

exista([H|_], H):- !.
exista([_|T], E):- exista(T, E).

/*
    sterge(L, E, R)
*/

sterge([], _, []):- !.
sterge([H|T], H, R):- sterge(T, H, R), !.
sterge([H|T], E, R):- sterge(T, E, R1),
                      R = [H|R1].

/*
    sterge_repetari(L, R)
*/

sterge_repetari([], []).
sterge_repetari([H|T], R):- exista(T, H), !,
                            sterge(T, H, R1),
                            sterge_repetari(R1, R).
sterge_repetari([H|T], R):- sterge_repetari(T, R1),
                            R = [H|R1].