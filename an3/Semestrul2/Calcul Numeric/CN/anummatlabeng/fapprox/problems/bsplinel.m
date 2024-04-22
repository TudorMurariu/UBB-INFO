function y=bsplinel(t,i,x)
%BSPLINEL - calcul valoare B-spline liniar
%apel y=bsplinel(x,k,t)
%t punctele
%i nr. b-spline-ului
%x nodurile

n=length(x);
y=zeros(size(t));
if i==1
    ig=find((t>=x(1))&(t<=x(2)));
    y(ig)=(x(2)-t(ig))/(x(2)-x(1));
elseif i==n
    ig=find((t>=x(n-1))&(t<=x(n)));
    y(ig)=(t(ig)-x(n-1))/(x(n)-x(n-1));
else
    ig1=find((t>=x(i-1))&(t<=x(i)));
    ig2=find((t>=x(i))&(t<=x(i+1)));
    y(ig1)=(t(ig1)-x(i-1))/(x(i)-x(i-1));
    y(ig2)=(x(i+1)-t(ig2))/(x(i+1)-x(i));
end

