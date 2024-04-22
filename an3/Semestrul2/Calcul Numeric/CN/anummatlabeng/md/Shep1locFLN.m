function z=Shep1locFLN(xp,yp,x,y,f,mu,R)
%computes locaShepard interpolant value in one point
% using  Franke-Little-Nielson weights
%call z=Shep1locFLN(xp,yp,x,y,f,mu,R)
% xp,yp - the point (which is different of each node)
% x,y - node coordinates
% f function value on nodes
% mu - exponent
% R - radius
n=length(x);
d=sqrt((xp-x).^2+(yp-y).^2);
w=zeros(size(f));
for k=1:n
   if (d(k)<R)
      w(k)=(prod(d([1:k-1,k+1:n]))*(R-d(k)))^mu;
   else
      w(k)=0;
   end
end
sw=sum(w);
z=sum(w.*f)/sw;