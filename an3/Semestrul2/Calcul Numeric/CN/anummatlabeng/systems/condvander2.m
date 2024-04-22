warning off
fprintf(' n   cond_inf     cond.estimate   theoretical\n')
for n=[10,20,40,80]
    t=linspace(-1,1,n);
    V=vander(t); 
    et=1/pi*exp(-pi/4)*exp(n*(pi/4+1/2*log(2)));
    x=[n, norm(V,inf)*norm(inv(V),inf), condest(V), et];
    fprintf('%d %e  %e  %e\n',x)
end
warning on