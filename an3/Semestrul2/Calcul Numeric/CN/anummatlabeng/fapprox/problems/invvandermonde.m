function U=invvandermonde(x)
n=length(x);
U=zeros(n);
for k=1:n
    p=poly(x([1:k-1,k+1:n]))/prod(x(k)-x([1:k-1,k+1:n]));
    U(:,k)=p';
end