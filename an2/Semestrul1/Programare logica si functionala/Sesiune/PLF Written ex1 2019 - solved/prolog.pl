/* 1. */

f([], 0).
f([H|T], S):- f(T, S1),
              S1 is S - H.

/*
    pe linia cu "S1 is S - H"
    S nu a fost legata inca si valoarea 
    S - H nu poate fi calculata

    pentru a calcula suma elementelor din lista
    am putea schimba al 2-lea predicat asa :

    f([H|T], S):- f(T, S1),
              S is S1 + H.
*/

/* 2. 

    invers_list(L, R)
    L : lista data
    R : lista raspuns

    Modele de flux: (i,i), (i,o)
*/

invers_list([], AUX, AUX).
invers_list([H|T], AUX, R):- is_list(H) ,!,
                        invers_list(H, [], L_small),
                        invers_list(T, [L_small|AUX] ,R).
invers_list([H|T], AUX, R):- invers_list(T, [H|AUX] ,R).

                        
main_invers_list(L, R):- invers_list(L, [], R).


/* 3. 

    subsets(L, N, R).
    
    Modele de flux: (i, i, i) determinist
    (i, i, o) nedeterminist
*/

subsets([H|T], 1, [H]).
subsets([_|T], N, R):- subsets(T, N, R).

subsets([H|T], N, R):- N1 is N - 1,
                       subsets(T, N1, R1),
                       R = [H | R1].
