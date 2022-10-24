/*
% elimina(l1..ln, poz){ [], n = 0}
                      { [l1 + elimina(l2..ln,X-1)], poz != 1 }
                      { l2..ln, poz == 1}



Predicat: elimina(L, N, R)
L - o lista de numere intregi
N - un numar care reprezinta pozitia elementului pe care il vom sterge
R - lista raspuns formata din lista L fara elementul de pe pozitia N
Modele de flux: (i, i, o) , (i, i, i) 


Teste:
elimina([], 4, R).
R = []
elimina([1, 2, 3, 4],1,R).
R = [2, 3, 4]
elimina([1, 2, 3, 4],4,R).
R = [1, 2, 3]
elimina([1, 2, 3, 4, 5, 6],4,R).
R = [1, 2, 3, 5, 6]
*/

elimina([], _, R):- R = [].

elimina([H|T], N, R):- N \= 1,
                       N1 is N - 1,
                       elimina(T, N1, R1),
                       R = [H|R1].

elimina([_|T], 1, T).