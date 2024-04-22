function [a,b,bcap,c,s,oop,fsal]=BS23
%initializare tabela Butcher pentru metoda BS23
%Bogacki-Shampine

fsal=1;
s=4;
q=3; p=2;
a=zeros(s,s-1);
a(2:s,1:s-1)=[1/2, 0, 0; 0, 3/4, 0; 2/9, 3/9, 4/9];
b=[7/24,6/24,8/24,3/24]';
bcap=[2/9,3/9,4/9,0]';
oop=1/(1+p);
c=sum(a');
%a(s,1:s-1)'-bcap(1:s-1)
fsal=sum(abs(a(s,1:s-1)'-bcap(1:s-1)))<1e-10;