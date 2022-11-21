/*
a. Sa se scrie un predicat care transforma o lista intr-o multime, in
    ordinea ultimei aparitii. Exemplu: [1,2,3,1,2] e transformat in [3,1,2].


    exista(L, X) -> returneaza true daca gaseste elementul in lista
    L : lista data
    X : element dat

    Modele de flux : (i,i)
*/

exista([], _):- !, false.

exista([H|_], X):- X = H, !.

exista([_|T], X):- exista(T, X).


/*

    build_multime(L, R)
    L : lista data
    R : multimea raspuns

    Modele de flux : (i,o) (i,i)
*/

build_multime([], []):- !.

build_multime([H|T], R):- exista(T, H), !.
                          build_multime(T, R).

build_multime([H|T], [H|R]):- not(exista(T, H)),
                              build_multime(T, R).                 