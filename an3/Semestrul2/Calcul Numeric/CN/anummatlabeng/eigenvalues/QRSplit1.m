function [lambda,It]=QRSplit1(A,t)
%QRSPLIT1 computes eigenvalues with QR and partition
%Input
%A - matrix
%t - tolerance
%Output
%lambda - eigenvalues
%It - no. of iterations

[m,n]=size(A);
if n>1
    H=hessen_h(A);   %convert to Hessenberg form
    [H1,H2,It]=QRIter(H,t); %decomposition H->H1,H2
    %recursive call
    [l1,It1]=QRSplit1(H1,t);
    [l2,It2]=QRSplit1(H2,t);
    It=It+It1+It2;
    lambda=[l1;l2];
else
    It=0;
    lambda=A;
end