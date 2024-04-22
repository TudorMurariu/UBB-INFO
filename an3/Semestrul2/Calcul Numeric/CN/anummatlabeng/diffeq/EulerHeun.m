function [a,b,bcap,c,s,oop,fsal]=EulerHeun
%initializare tabela Butcher pentru problema 10.13
%Bogacki-Shampine

fsal=1;
s=2;
q=1; p=1;
a(2,1)=1;
b=[1/2,1/2]';
bcap=[1,0]';
oop=1/(1+p);
c=[0,1];
%a(s,1:s-1)'-bcap(1:s-1)
fsal=sum(abs(a(s,1:s-1)'-bcap(1:s-1)))<1e-10;
