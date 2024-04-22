function z=SheplocFLN(xp,yp,x,y,f,mu,R)
%computes local Shepard interpolant values on a grid
%with Franke-Little-Nielson weights
% call z=SheplocFLN(xp,yp,x,y,f,mu,R)
% xp,yp - the points
% x,y - node coordinates
% f function value on nodes
% mu - exponent
%R - the radius
if size(xp)~=size(yp)
   error('xp and yp have not the same size')
end
[m,n]=size(xp);
for i=1:m
   for j=1:n
      z(i,j)=Shep1locFLN(xp(i,j),yp(i,j),x,y,f,mu,R);
   end
end
