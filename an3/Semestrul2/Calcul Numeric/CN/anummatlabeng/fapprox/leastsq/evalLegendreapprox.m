function y=evalLegendreapprox(c,x)
%EVALLEGENDREAPPROX - evaluate least squares Legendre approximation

y=zeros(size(x));
for k=1:length(c)
    y=y+c(k)*vLegendre(x,k-1);
end