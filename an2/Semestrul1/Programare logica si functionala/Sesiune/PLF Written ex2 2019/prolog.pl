/*
    1.

    Original:
    f([], -1).
    f([H|T], S):- f(T,S1),
                S1 > 0, !,
                S is S1 + H.
    f([_|T], S):- f(T,S1),
                S is S1.
    Solutie:
*/

f([], -1).
f([H|T], S):- f(T,S1), 
              aux(S1, S, H).

aux(S1, S, H):- S1 > 0, !,
                S is S1 + H.
aux(S1, S1, _).

/*
    2.

    exista(R, E).

    set(L, R).
*/

exista([H|_], H):- !.
exista([_|T], E):- exista(T, E).

set([], []).
set([H|T], R):- set(T, R),
                exista(R, H), !.

set([H|T], R):- set(T, R1),
                R = [H|R1].


/*
    3.

    alege(L, E)

    aranjamenteP(L, N, P, Raux, P1)
*/

alege([H|_], H).
alege([H|T], X):- alege(T, X).

aranjamenteP(_, 0, P, Raux, P, Raux).
aranjamenteP(L, N, P, Raux, P1, R):- N > 0,
                                     P1 < P,
                                     N1 is N - 1,
                                     alege(L, X),
                                     P2 is P1 * X,
                                     Raux1 = [X|Raux],
                                     aranjamenteP(L, N1, P, Raux1, P2, R).

main(L, N, P, R):- aranjamenteP(L, N, P, [], 1, R).