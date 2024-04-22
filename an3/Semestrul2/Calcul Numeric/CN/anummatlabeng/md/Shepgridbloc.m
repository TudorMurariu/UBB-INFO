function z=Shepgridbloc(xp,yp,x,y,f,mu,R)
%computes local Shepard interpolant values on a grid
% using the barycentric form and Franke-Little weights
% call z=Shepgridbloc(xp,yp,x,y,f,mu,R)
% xp,yp - the points
% x,y - node coordinates
% f function value on nodes
% mu - exponent
% R - Radius
if size(xp)~=size(yp)
   error('xp and yp have not the same size')
end
[m,n]=size(xp);
for i=1:m
   for j=1:n
      z(i,j)=Shep1barloc(xp(i,j),yp(i,j),x,y,f,mu,R);
   end
end
