# Test 5 Sisteme , Problema 13
# generare matrice Hessenberg superioara
n = 5;
A = randi(100, n);
A=triu(A,-1);
b=A*ones(n, 1);

[L1, U1, P1] = lup(A);
[L2, U2, P2] = lup_hessenberg(A);
L1*U1 == P1*A
L2*U2 == P2*A

%solutia folosind LUP
y1 = L1 \ (P1 * b);
x1 = U1 \ y1 

%solutia folosind LUP Hessenberg
y2 = L2 \ (P2 * b);
x2 = U1 \ y2

format long
norm(P1*A-L1*U1)
norm(P2*A-L2*U2)
% Se observa ca normele sunt apropiate de 0, 
% deci descompunerile gasite sunt bune.