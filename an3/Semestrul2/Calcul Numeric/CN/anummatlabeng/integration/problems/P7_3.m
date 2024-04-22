%P7_3
%precizia eps (double) pt Simpson
%eps(single), trapeze
clear
f=@(x) 2./(1+x.^2);

k=1;
I2=Simpson(f,-1,1,k);
prec2(k)=abs(pi-I2)/pi;
while prec2(k)>=eps
    k=k+1;
    I2=Simpson(f,-1,1,k);
    prec2(k)=abs(pi-I2)/pi;
end
tt2=1:k;
subplot(2,1,1)
semilogy(tt2,prec2,'k-',tt2,eps*ones(size(tt2)),'b--')
s1=sprintf('Simpson,n=%d',tt2(end));
title(s1);

k=100; tt1=k;
I1=trapez(f,-1,1,k);
prec1=abs(pi-I1)/pi;
while prec1(end)>=eps('single')
    if k< 1300
        k=k+100;
    else
        k=k+1;
    end
    tt1=[tt1,k];
    I1=trapez(f,-1,1,k);
    prec=abs(pi-I1)/pi;
    prec1=[prec1,prec];
end
subplot(2,1,2)
semilogy(tt1,prec1,'k-',tt1,eps('single')*ones(size(tt1)),'b--')
s2=sprintf('trapeze,n=%d',tt1(end));
title(s2);

