/*
    1.

    power(X, N, AUX, R) - in O(n)
    power2(X, N, AUX, R) - in O(log n)
*/

power(X, N, R):- power(X, N, 1, R).
power(_, 0, AUX, AUX).
power(X, N, AUX, R):- N > 0,
                      N1 is N-1,
                      AUX1 is AUX * X,
                      power(X, N1, AUX1, R).


power2(X, N, R):- power2(X, N, 1, R).
power2(_, 0, AUX, AUX).
power2(X, N, AUX, R):- N > 0,
                       0 is N mod 2, !,
                       X1 is X * X,
                       N1 is N / 2,
                       power2(X1, N1, AUX, R).

power2(X, N, AUX, R):- N > 0,
                       N1 is N-1,
                       AUX1 is AUX * X,
                       power2(X, N1, AUX1, R).