function z=Shep1pt(xp,yp,x,y,f,mu)
%SHEP1PT - computes Shepard interpolant value in one point
%call Z=SHEP1PT(XP,YP,X,Y,F,MU)
% XP,YP - the point
% X,Y - node coordinates
% F function value on nodes
% MU - exponent
d=(sqrt((xp-x).^2+(yp-y).^2)).^mu;
n=length(x);
A=zeros(size(f));
for i=1:n
   A(i)=prod(d([1:i-1,i+1:n]));
end
z=sum(A.*f)/sum(A);
