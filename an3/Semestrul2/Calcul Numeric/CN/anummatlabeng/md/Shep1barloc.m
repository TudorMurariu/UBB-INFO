function z=Shep1barloc(xp,yp,x,y,f,mu,R)
%computes locaal Shepard interpolant value at one point
% using the barycentric form and Franke-Little weights
%call z=Shep1barloc(xp,yp,x,y,f,mu,R)
% xp,yp - the point (which is different of each node)
% x,y - node coordinates
% f function value on nodes
% mu - exponent
% R - radius
d=(xp-x).^2+(yp-y).^2;
n=length(x);
w=zeros(1,n);
ix=(d<R);
w(ix)=((R-d(ix))./(R*d(ix))).^mu;
z=sum(w*f)/sum(w);