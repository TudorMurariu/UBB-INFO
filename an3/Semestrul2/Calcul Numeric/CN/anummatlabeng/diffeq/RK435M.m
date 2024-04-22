function [a,b,bcap,c,s,oop,fsal]=RK435M
%initializare tabela Butcher pentru RK4(3)5M

fsal=0;
s=5;
q=4; p=3;
a=zeros(s,s-1);
a(2:s,1:s-1)=[1/5, 0, 0, 0; 0, 2/5, 0, 0; ...
        6/5, -12/5, 2, 0; -17/8, 5, -5/2, 5/8];
bcap=[13/96, 0, 25/48, 25/96, 1/12]';
b=[23/192, 0, 55/96, 35/192, 1/8]';
oop=1/(1+p);
c=sum(a');
fsal=sum(abs(a(s,1:s-1)'-bcap(1:s-1)))<1e-10;