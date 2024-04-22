function flag=testdd(A)
%TESTDD - testeaza daca A este diagonal dominanta
B=A-diag(diag(A));
flag=all(sum(B,2)<diag(A));