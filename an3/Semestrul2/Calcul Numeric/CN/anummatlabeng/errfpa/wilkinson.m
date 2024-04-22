function Wilkinson(n)
%perturbations for Wilkinson example

p=poly(1:n);
h=plot([1:n],zeros(1,n),'.');
set(h,'Markersize',15);
hold on
for k=1:1000
    r=randn(1,n+1);
    pr=p.*(1+1e-10*r);
    z=roots(pr);
    h2=plot(z,'k.');
    set(h2,'Markersize',4)
end
axis equal