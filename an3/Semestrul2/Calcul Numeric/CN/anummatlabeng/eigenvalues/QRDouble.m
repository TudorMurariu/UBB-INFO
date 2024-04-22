function [H1,H2,It]=QRDouble(H,t)
%QRDOUBLE - perform double step QR iteration and inverse transform on
%Hessenberg matrix until the least subdiagonal element is < t
%Input 
% H - Hessenberg matrix
% t - tolerance
%Output
%H1, H2 - descomposition over least element
%It - no. of iterations

It=0; [m,n]=size(H);
II=eye(n);
[m,j]=min(abs(diag(H,-1))); 
while m>t
    It=It+1;
    X = H*H ... % X matrix
        - (H(n-1,n-1) + H(n,n)) * H ...
        + (H(n-1,n-1)*H(n,n) - H(n,n-1)*H(n-1,n))*II;
    [Q,R]=qr(X);
    H=hessen_h(Q'*H*Q);
    [m,j]=min(abs(diag(H,-1)));
end
H1=H(1:j,1:j);
H2=H(j+1:n,j+1:n);
