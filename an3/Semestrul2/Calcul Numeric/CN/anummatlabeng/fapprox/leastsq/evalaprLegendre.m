function y=evalaprLegendre(c,x)
%EVALAPRLEGENDRE - evaluare aproximanta Legendre mcmmp

y=zeros(size(x));
for k=1:length(c)
    y=y+c(k)*vLegendre(x,k-1);
end