/*

    sterge(L, X, R)
*/

sterge([], _, []).

sterge([X|T], X, R):- !, sterge(T, X, R).

sterge([H|T], X, R):-  sterge(T, X, R1),
                       R = [H|R1].