%P6_8 Operatorul Favard-Szasz

ft=@(x) 1./(1+x.^2);
limsup=input('limita superioara:');
nrt=input('numar de termeni:');
x=linspace(0,limsup,200);
i=0;
ss=cell(6,1); 
cc={'b-','g-','y-','k-','m-'};
for m=10:10:50
    s=zeros(size(x));
    u=ones(size(x));
    for k=0:nrt
        u=(m*x).^k/gamma(k+1).*exp(-m*x)*ft(k/m);
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
title('operatorul Favard-Szasz')
hold off
  
