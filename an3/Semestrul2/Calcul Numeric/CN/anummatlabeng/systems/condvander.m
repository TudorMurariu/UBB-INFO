warning off
fprintf(' n   cond_inf     cond.estimate   theoretical\n')
for n=10:15
    t=1./(1:n);
    V=vander(t);
    x=[n, norm(V,inf)*norm(inv(V),inf), condest(V), n^(n+1)];
    fprintf('%d %e  %e  %e\n',x)
end
warning on