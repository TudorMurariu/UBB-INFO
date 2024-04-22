%P9_1
for n=11:20
    H=hilb(n);
    n
    e=eig(H)'
    ce=condeig(H)'
    pause
end