fd=@(t,y) y^2-t;
np=250;
t=linspace(0,3,np);
y=zeros(np,6);
y0=-5:0;
for k=1:length(y0)
    sol=ode45(fd,[0,8],y0(k));
    y(:,k)=deval(sol,t);
end
plot(t,y)