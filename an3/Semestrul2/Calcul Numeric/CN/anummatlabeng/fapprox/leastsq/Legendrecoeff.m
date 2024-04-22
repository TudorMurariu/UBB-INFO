function c=Legendrecoeff(f,n)
%LEGENDRECOEFF - coefficients of least squares Legendre approximation
%call c=Legendrecoeff(f,n)
%f - function
%n - degree

n3=2; 
for k=0:n
    if k>0, n3=n3*k^2/(2*k-1)/(2*k+1); end
    c(k+1)=quadl(@fleg,-1,1,1e-12,0,f,k)/n3;
end
%subfunction
function y=fleg(x,f,k)
y=f(x).*vLegendre(x,k);
