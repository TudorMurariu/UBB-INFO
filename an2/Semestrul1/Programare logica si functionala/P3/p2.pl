/*
  divisible(X, Y)  =  { true , X mod Y = 0
                      { divisible(X, Y+1), X mod Y != 0 si X > Y+1
                      { false, altfel

  X - numar natural a caruia ii verificam divizibilitatea
  Y - numar natural
  Modele de flux : (i, i)
*/

divisible(X, Y):- 0 is X mod Y, !.
divisible(X, Y):- X > Y+1,
                  divisible(X, Y+1).

/*
prim(X) = { true, X = 2
          { false, X < 2
          { not(divisible(X, 2)), altfel

    X - numarul intreg pentru care verificam daca este prim
    Modele de flux : (i)

*/

prim(2):- true, !.
prim(N):- N < 2, !,
          false.

prim(N):- not(divisible(N, 2)).


/*

primList(n) = { [], n <= 1 }
              { primList(n-1), not(prim(n)) }
              { n + primList(n-1), prim(n) }

 primList(N, R)
 N - un numar intreg dupa care formam lista de nr prime
 R - lista raspuns formata din toate nr prime mai mici decat N

 Modele de flux : (i,o) (i,i)

*/

primList(N, []):- N =< 1, !.

primList(N, R):- not(prim(N)), !,
                 N1 is N - 1,
                 primList(N1, R).


primList(N, R):- prim(N), !,
                 N1 is N - 1,
                 primList(N1, R1),
                 R = [N|R1].


/*

candidat(l1...ln) = 
          1) l1, n>=1
          2) candidat(l2..ln) , n>=2

  candidat(L, E)
  L - lista din care luam un candidat
  E candidatul returnat

  Modele de flux : (i,o) (i,i)

*/

candidat([H|_], H).

candidat([_|T], E):- candidat(T, E).


/*
solutii(N) =  
    1) {A, B} ,  A,B din primList(N) si A+B = N

 solutii(N, A, B)
 N - numar dat
 A - nr prim
 B - nr prim
 
*/

solutii(N, A, B):- primList(N, R),
                   candidat(R, A),
                   candidat(R, B),
                   N is A + B.