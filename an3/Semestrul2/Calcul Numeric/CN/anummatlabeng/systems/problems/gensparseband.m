function A=gensparseband(n,p,q)
%GENSPARSEBAND - genereaza o matrice banda rara diagonal dominanta
%apel A=gensparseband(n,p,q)
%n - dimensiunea
%p - semilatime banda superioara
%q - semilatime banda inferioara
%A - rezultat

d=[-q+1:0,1:p-1];
B=rand(n,length(d));
B(:,q)=5*sum(abs(B),2);
A=spdiags(B,d,n,n);
