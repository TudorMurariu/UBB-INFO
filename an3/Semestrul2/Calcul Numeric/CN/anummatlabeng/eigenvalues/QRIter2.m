function [H1,H2,It]=QRIter2(H,t)
%QRITER - perform QR iteration on Hessenberg matrix 
%until the least subdiagonal element is < t
%Input 
% H - Hessenberg matrix
% t - tolerance
%Output
%H1, H2 - descomposition over least element
%It - no. of iterations

It=0; [m,n]=size(H);
II=eye(n);
[m,j]=min(abs(diag(H,-1))); 
while m > t
    It=It+1;
    H=HessenRQ(H-H(n,n)*II)+H(n,n)*II;
    [m,j]=min(abs(diag(H,-1)));
end
H1=H(1:j,1:j);
H2=H(j+1:n,j+1:n);
