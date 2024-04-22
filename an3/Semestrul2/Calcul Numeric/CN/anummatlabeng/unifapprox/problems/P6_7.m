%P6_7 Operatorul lui Baskakov

ft=@(x) 1./(1+x.^2);
limsup=input('limita superioara:');
nrt=input('numar de termeni:');
x=linspace(0,limsup,300);
i=0;
ss=cell(6,1); 
cc={'b-','g-','y-','k-','m-'};
for m=5:5:25
    s=zeros(size(x));
    u=ones(size(x));
    for k=0:nrt
        u=nchoosek(m+k-1,k)*x.^k./(1+x).^(m+k)*ft(k/m);
        s=s+u;
    end
    i=i+1;
    plot(x,s,cc{i}); hold on
    ss{i}=sprintf('m=%d',m);
end
i=i+1;
ss{i}='functia';
plot(x,ft(x),'r-')
legend(ss,'Location','EastOutside')
title('operatorul lui Baskakov')
hold off
  
