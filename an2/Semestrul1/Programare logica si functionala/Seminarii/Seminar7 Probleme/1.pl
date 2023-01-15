/*1. Se consideră problema ștergerii dintr-o listă a tuturor elementele care se repetă.
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


/* 2.Se consideră problema generării tuturor combinărilor luate a câte N, 
cu elementele unei listedate.

    comb(L: lista, N: integer, R:lista)

1 2 3
2 ->  1 2,  1 3,  2 3
*/

comb([E|_], 1, [E]).
comb([_|T], N, R):- comb(T, N, R).
/*comb([H|T], N, [H|R]):- N > 1,
                        N1 is N - 1,
                        comb(T, N1, R). 

 Să presupunem că nu vrem toate combinările, 
ci doar cele în care elementele sunt în ordine crescătoare
*/
comb([H|T], N, [H,H1|R]):- N > 1,
                        N1 is N - 1,
                        comb(T, N1, [H1|R]),
                        H1 > H.


/*3. La teme de laborator apare problema următoare: 
să se ștearga 1-ul, al 3-lea, al 7-lea, etc. element dintr-o listă.

    elimina(L, poz_actuala, poz_stergere, R)
*/

elimina(L, R):- elimina(L, 1, 1, R).
elimina([], _, _, []).
elimina([_|T], X, X, R):-   X_Next is 2*X+1,
                            A1 is X + 1,
                            elimina(T, A1, X_Next, R), !.

elimina([H|T], A, X, [H|R]):-  A1 is A + 1,
                               elimina(T, A1, X, R).

/*elimina([1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16],R), print(R), nl.
    ca sa afiseze toata lista in swi prolog.
*/