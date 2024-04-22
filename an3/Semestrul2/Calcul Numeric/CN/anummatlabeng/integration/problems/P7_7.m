%P7_7
syms x f u
f=log(1+x)*log(1-x);
u=int(f,-1,1)
vd=double(u)
clear x
ff =@(x) log(1+x)*log(1-x);
adquad(ff,-1+eps/2,1-eps/2)
er=[]; nev=[]; tt=[];
for tol=-1:-1:-16
    [I,nef]=adquad(ff,-1+eps/2,1-eps/2,10^tol);
    tt=[tt;abs(tol)];
    er=[er;abs(I-vd)/abs(vd)];
    nev=[nev;nef];
end
subplot(2,1,1)
plot(tt,nev,'d-');
title('numar de evaluari')
xlabel('toleranta(10^{-k})')
ylabel('nr. evaluari')
subplot(2,1,2)
semilogy(tt,er,'x-')
title('eroarea')
xlabel('toleranta(10^{-k})')
ylabel('eroarea')

        
    