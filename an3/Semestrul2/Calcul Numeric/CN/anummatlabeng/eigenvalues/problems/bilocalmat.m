function A=bilocalmat(p,q,r,a,b,N)
%BILOCALMAT - matrix for two points BVP
%y''(x)-p(x)y'(x)-q(x)y(x)=r(x), x in [a,b]
%y(a)=alfa, y(b)=beta
%call y=bilocal(p,q,r,a,b,alfa,beta,N)
%p,q,r - functions
%[a,b] - interval
%N - no. of points

h=(b-a)/(N+1); x=a+[1:N]'*h;
vp=p(x); vr=r(x); vq=q(x);
av=1+h^2/2*vq;
bv=1/2*(1+h/2*vp);
cv=1/2*(1-h/2*vp);
B=[[-bv(2:end);0],av,[0;-cv(1:end-1)]];
A=spdiags(B,[-1:1],N,N);
