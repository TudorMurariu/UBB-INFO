function z=Shepgrid(xp,yp,x,y,f,mu)
%SHEPGRID - computes Shepard interpolant values on a grid
% call Z=SHEPGRID(XP,YP,X,Y,F,MU)
% XP,YP - points
% X,Y - node coordinates
% F function value on nodes
% MU - exponent
if size(xp)~=size(yp)
   error('xp and yp have not the same size')
end
[m,n]=size(xp);
for i=1:m
   for j=1:n
      z(i,j)=Shep1pt(xp(i,j),yp(i,j),x,y,f,mu);
   end
end
