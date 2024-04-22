fprintf(' n   cond_2       est. cond      theoretical\n')
for n=[10:15,20,40]
    H=hilb(n);
    et=(sqrt(2)+1)^(4*n+4)/(2^(14/4)*sqrt(pi*n));
    x=[n, norm(H)*norm(invhilb(n)), condest(H), et];
    fprintf('%d %g  %g  %g\n',x)
end