function A=P1_6(n)
%P1_6 - generare matrice tridiagonala
A=diag([1:n])-diag([2:n],-1)+diag([n:-1:2],1);