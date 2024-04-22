function [lambda,it]=QRMethod2(A,t)
%QRMETHOD2 - Computes eigenvalues of a real matrix with QR method and shift
%Input
%    A - matrix
%    t - tolerance
%Output
%    lambda - eigenvalues - diagonal of R
%    it - no. of iterations

[m,n]=size(A); II=eye(n);
H=hessen_h(A);
it=0;

while norm(diag(H,-1),inf) > t
    m = H(n,n);
    H = HessenRQ(H - m * II) + m*II;
    it=it+1;
end
lambda=diag(H);
    
