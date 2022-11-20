/*

    exista finctia predefinita reverse care inverseaza o lista


    add_numbers_inv(l1..ln, e1..em, rest) = { [] , n = 0 si m = 0  }
                { (e1+rest)%10 +  add_numbers_inv([], e1..em, (e1+rest)/10), n = 0 }
                { (l1+rest)%10 +  add_numbers_inv(l1..ln, [], (l1+rest)/10) , m = 0 }
                { (e1+l1+rest)%10 +  add_numbers_inv(l1..ln, e1..em, (e1+l1+rest)/10), altfel }

    add_numbers_inv(L1, L2, R, Rest)
    L1 : prima lista
    L2 : a 2-a lista
    R : lista raspuns
    Rest : este un numar intreg

    Modele de flux : (i,i,o,i) (i,i,i,i)
*/

add_numbers_inv(L1, L2, R):- add_numbers_inv(L1, L2, R, 0), !.

add_numbers_inv([], [], [], _):- !.

add_numbers_inv([], [H|T], R, Rest):- Nou_Rest is (H + Rest) // 10, 
                                      add_numbers_inv([], T, R1, Nou_Rest),
                                      Prim is (H + Rest) mod 10, 
                                      R = [Prim|R1].


add_numbers_inv([H|T], [], R, Rest):- Nou_Rest is (H + Rest) // 10, 
                                      add_numbers_inv(T, [], R1, Nou_Rest),
                                      Prim is (H + Rest) mod 10, 
                                      R = [Prim|R1].

add_numbers_inv([H|T], [H1|T1], R, Rest):- Nou_Rest is (H + H1 + Rest) // 10, 
                                           add_numbers_inv(T, T1, R1, Nou_Rest),
                                           Prim is (H + H1 + Rest) mod 10, 
                                           R = [Prim|R1].
/*

    add_numbers(l1..ln, e1..en) = 
            { reverse(add_numbers_inv(reverse(l1..ln), reverse(e1..en))) }

    add_numbers(L1, L2, R)
    L1 : prima lista
    L2 : a 2-a lista
    R : lista raspuns
    
    Modele de flux : (i,i,o) (i,i,i)
*/

add_numbers(L1, L2, R):- reverse(L1, L11),
                         reverse(L2, L22),
                         add_numbers_inv(L11, L22, R1),
                         reverse(R1, R).