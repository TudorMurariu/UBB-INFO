function c=coeffChebyshevdiscr(f,n)
%COEFFCHEBYSHEVDISCR - coeficienti Cebisev mcmmp discreti
%apel c=coeffChebyshevdiscr(f,n)
%f - functia
%n - gradul

xi=cos((2*[1:n+1]-1)*pi/(2*n+2));
y=f(xi)';
for k=1:n+1
    c(k)=2/(n+1)*vChebyshev(xi,k-1)*y;
end