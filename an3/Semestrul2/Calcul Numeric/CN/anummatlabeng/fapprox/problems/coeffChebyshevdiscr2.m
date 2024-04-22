function c=coeffChebyshevdiscr2(f,n)
%COEFFCHEBYSHEVDISCR2 - coeficienti Cebisev mcmmp discreti
%cu punctele de extrem
%apel c=coeffChebyshevdiscr(f,n)
%f - functia
%n - gradul

eta=cos([0:n]*pi/n);
for k=1:n+1
    c(k)=2/n*prscal(vChebyshev(eta,k-1),f(eta));
end
function p=prscal(f,g)
%produs scalar in puncte de extrem
p=1/2*(f(1)*g(1)+f(end)*g(end));
p=p+dot(f(2:end-1),g(2:end-1));