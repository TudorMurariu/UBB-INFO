function z=Newtonpol(td,x,t)
%NEWTONPOL - computes Newton interpolation polynomial
%call z=Newtonpol(td,x,t)
%td - divided difference table
%x - interpolation nodes
%t - evaluation points
%z - values of interpolation polynomial

lt=length(t); lx=length(x);
for j=1:lt
   d=t(j)-x;
   z(j)=[1,cumprod(d(1:lx-1))]*td(1,:)';
end
