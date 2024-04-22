function x=lupsolve(A,b)
%LUPSOLVE - solution of an algebraic system by LUP decomposition

[L,U,P]=lup(A);
y=forwardsubst(L,P*b);
x=backsubst(U,y);