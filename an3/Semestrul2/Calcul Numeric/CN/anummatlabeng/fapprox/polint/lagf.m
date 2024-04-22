function fi=lagf(x,xi,k)
%calculeaza valoarea polinomului fundamental Lagrange lk
%x - nodurile
%xi - punctele in care se evalueaza polinomul fundamental
%k - indicele polinomului
if nargin~=3
error('numar ilegal de argumente')
end;
fi=ones(size(xi));
np1=length(xi);
for j=1:np1
  if j~=k
   fi=fi.*(xi-x(j))/(x(k)-x(j));
  end;
end;