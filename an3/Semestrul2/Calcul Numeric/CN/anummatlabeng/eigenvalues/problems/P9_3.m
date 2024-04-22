%P9_3
for k=3:20
    M=magic(k);
    v(k)=max(abs(eig(M)));
    ss=sum(M);
    s(k)=ss(1);
    fprintf('k=%d, vemax=%g, %g\n',k,v(k),s(k))
end