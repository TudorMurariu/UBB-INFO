function P5_2
%P5_2 - problema 5_2
for N=[5,10,20,4]
    for n=1:5
        [nc,c,ermin,ermax]=P5_2b(n,N);
        fprintf('n=%d,N=%d\n',n,N)
        fprintf('nr. cond=%f\n',nc);
        disp('coeficientii')
        c
        fprintf('Eroarea\n  min=%f max=%f\n',ermin,ermax);
        pause
    end
end

function [nc,c,ermin,ermax]=P5_2b(n,N)
t=[1:N]'/(N+1); b=zeros(n,1);
f=sin(pi/2*t); 
for i=1:n
    b(i)=dot(pij(t,i),f-t);
    for j=1:n
        A(i,j)=dot(pij(t,i),pij(t,j));
    end
end
c=A\b;
nc=cond(A);
fin=t+t.*(1-t).*polyval(c(end:-1:1),t);
ermin=min(abs(fin-f));
ermax=max(abs(fin-f));
t=linspace(0,1,100);
f=sin(pi/2*t);
fin=t+t.*(1-t).*polyval(c(end:-1:1),t);
plot(t,f,t,fin);
legend('f','aproximanta','Location','Best')
tt=sprintf('n=%d,N=%d\n',n,N)
title(tt)


function y=pij(t,j)
y=t.*(1-t).*t.^(j-1);
