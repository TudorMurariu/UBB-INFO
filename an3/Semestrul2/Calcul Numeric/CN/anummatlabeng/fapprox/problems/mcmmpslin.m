function c=mcmmpslin(f,x)
%MCMMPSLIN - spline liniare prin mcmmp

%construiesc matricea
dx=diff(x);
dd=[dx(1),dx(1:end-1)+dx(2:end),dx(end)];
C=1/3*diag(dd)+1/6*diag(dx,1)+1/6*diag(dx,-1);
n=length(x); b=zeros(n,1);
b(1)=quad(@bff,x(1),x(2),1e-6,[],f,1,x);
for k=2:n-1
   b(k)=quad(@bff,x(k-1),x(k+1),1e-6,[],f,k,x);
end
b(n)=quad(@bff,x(n-1),x(n+1),1e-6,[],f,n,x);
c=C\b;
   
function y=bff(x,f,k,t)
%calcul membru drept
y=feval(f,x).*bsplinel(x,k,t);