function  Z=pfl2(x,t)
%PFL2 - computes basic Lagrange polynomials
%call Z=pfl2(x,t)
%x - interpolation nodes
%t - evaluation points

m=length(x);
n=length(t);
[T,X]=meshgrid(t,x);
TT=T-X;
Z=zeros(m,n);
TX=zeros(m,m);
[U,V]=meshgrid(x,x);
XX=U-V;
for i=1:m
   TX(i)=prod(XX([1:i-1,i+1:m],i));
   Z(i,:)=prod(TT([1:i-1,i+1:m],:))/TX(i);
end