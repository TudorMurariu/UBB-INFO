function [a,b,c,s]=RK4tab
%RK4 - Butcher table for classical RK4
s=4;
a=zeros(s,s-1);
a(2:s,1:s-1)=[1/2,0,0; 0, 1/2,0; 0,0,1];
b=[1,2,2,1]'/6;
c=sum(a');