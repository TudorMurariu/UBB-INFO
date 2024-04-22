function [lambda,it]=QRMethod1(A,t)
%QRMETHOD1 - Computes eigenvalues of a real matrix
%naive implementation
%Input
%    A - matrix
%    t - tolerance
%Output
%    lambda - eigenvalues - diagonal of R
%    it - no. of iterations

H=hessen_h(A);
it=0;
while norm(diag(H,-1),inf) > t
    H=HessenRQ(H);
    it=it+1;
end
lambda=diag(H);
    
