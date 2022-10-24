
prim(2).
prim(N):- prim(N, 2).

prim(N, X):- X < N // 2,
            prim(N, X)