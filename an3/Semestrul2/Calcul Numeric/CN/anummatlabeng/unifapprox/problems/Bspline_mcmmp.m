function [c,v]=Bspline_mcmmp(k,n,kn,f,t)
%BSPLINE_MCMMP Aproximare mcmmp cu B-spline
%apel [c,t,v]=Bspline_mcmmp(k,n,f)
%c - coeficientii
%t - abscisele
%v -valorile
%k - gradul
%kn - nodurile

aa=min(kn); bb=max(kn); %limitele
d=-k:k;
B=zeros(n+1,2*k+1);
b=zeros(n+1,1);
for ncol=0:k
    for nlin=ncol:n
        B(nlin+1,ncol+k+1)=quadl(@ps,aa,bb,1e-6,0,nlin,nlin-ncol,k,kn);
    end
end
for i=0:n
    b(i+1)=quadl(@md,aa,bb,1e-6,0,i,f,k,kn);
end
for ncol=-1:-1:-k
    B(1:n+1+ncol,k+1+ncol)=B(abs(ncol)+1:n+1,abs(ncol)+k+1);
end
A=spdiags(B,d,n+1,n+1);
c=A\b;
v=Cox_deBoor(k,c',kn,t);

function y=ps(x,i,j,k,kn)
y=Bspline(k,kn,i,x).*Bspline(k,kn,j,x);

function y=md(x,i,f,k,kn)
y=Bspline(k,kn,i,x).*f(x);