/*
  divisible(X, Y)  =  { true , X%Y = 0
                      { divisible(X, Y+1), X%Y != 0 si X > Y+1
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

afisare_descompunere_prime(N) = afisare_descompunere_prime(2, N-2,N) 

afisare_descompunere_prime(A, B, N) =      { afiseaza(A, B) si 
                                           afisare_descompunere_prime(A+1, B, N) si 
                                           afisare_descompunere_prime(A, B-1, N)
                                           , daca A+B=N si prim(A) si prim(B)

                                        { afisare_descompunere_prime(A+1, B, N) si 
                                           afisare_descompunere_prime(A, B-1, N)
                                           , altfel
    
    afisare_descompunere_prime(N).
    N - numar interg
    
    Modele de flux : (i) 

*/

afisare_descompunere_prime(N):- N > 3, !,
                                N1 is N - 2,
                                afisare_descompunere_prime(2, N1,N).


afisare_descompunere_prime(A, B, N):- A < N,
                                      B > 1,
                                      N is A + B,
                                      prim(A),
                                      prim(B), !,
                                      write(A, B),
                                      A1 is A + 1,
                                      B1 is B - 1,
                                      afisare_descompunere_prime(A1, B, N),
                                      afisare_descompunere_prime(A, B1, N).


afisare_descompunere_prime(A, B, N):- A < N,
                                      B > 1, !,
                                      A1 is A + 1,
                                      B1 is B - 1,
                                      afisare_descompunere_prime(A1, B, N),
                                      afisare_descompunere_prime(A, B1, N).