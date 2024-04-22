function  Z=pfl2b(x,t)
%PFL2b - compute basic Lagrange polynomials
%call Z=PFL2B(x,t)
%X - interpolation nodes
%T - evaluation points 

m=length(x);
n=length(t);
[T,X]=meshgrid(t,x);
TT=T-X;
Z=zeros(m,n);
[U,V]=meshgrid(x,x);
XX=U-V;
for i=1:m
   TX=prod(XX([1:i-1,i+1:m],i));
   Z(i,:)=prod(TT([1:i-1,i+1:m],:))/TX;
end