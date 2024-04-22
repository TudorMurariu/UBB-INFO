%THT - test heat transfer
c = [1e-8, 1e-7, 1e-6];
n=30;
x=zeros(n+2,3);
y=x;
L=1;

for k=1:length(c)
    [x(:,k),y(:,k)]=HeatTransfer(L,c(k),n);
end
plot(x(:,1),y(:,1),'k-',x(:,2),y(:,2),'k--',...
    x(:,3),y(:,3),'k:');
legend('1e-8','1e-7','1e-6',0)


