function wilkinsonu(n)
%perturbatie pentru \_prod_{k=1}^{n}(x-k) uniforma
p=poly(1:n);
h=plot([1:n],zeros(1,n),'.');
set(h,'Markersize',15);
hold on
for k=1:1000
    r=(1e-10)*rand(1,n+1);
    pr=p+r.*p; %.*(1+1e-10*r);
    z=roots(pr);
    h2=plot(z,'k.');
    set(h2,'Markersize',4)
end