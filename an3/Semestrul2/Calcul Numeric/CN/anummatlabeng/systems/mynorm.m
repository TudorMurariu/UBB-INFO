function Z=mynorm(X,Y,p)
n=length(X(:));
Z=zeros(size(X));
if isfinite(p)
    Z=(abs(X).^p+abs(Y).^p).^(1/p);
else
    for i=1:n
        Z(i)=norm([X(i),Y(i)],p);
    end
end