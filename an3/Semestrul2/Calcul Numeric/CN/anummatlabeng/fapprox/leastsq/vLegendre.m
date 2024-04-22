function vl=vLegendre(x,n)
%VLEGENDRE - value of Legendre polynomial
%call vl=vLegendre(x,n)
%x  - points
%n  - degree
%vl - value

pnm1 = ones(size(x));
if n==0, vl=pnm1; return; end 
pn = x;
if n==1, vl=pn; return; end 
for k=2:n
    vl=x.*pn-1/(4-(k-1)^(-2)).*pnm1;
    pnm1=pn; pn=vl;
end