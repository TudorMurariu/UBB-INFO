%GAS
%set parameters
alpha=188.33;
beta_p=9.77e-4;
P=1013250;
R=8.3144721515/0.044;
%R=11.0163766063776
T=300;
%plot f
f = @(v) (P+alpha./v.^2).*(v-beta_p)-R*T;
fd=  @(v) P+alpha./v.^2-2*(v-beta_p)*alpha./v.^3;
v=linspace(1e-3,0.1,5000);
plot([0,0.1],[0,0],'k-',v,f(v),'k-')
%Newton method
for v0=[1e-4,1e-3,1e-2,1e-1]
    [vv,nit]=Newton(f,fd,v0,0,eps,200);
    v0,vv,nit
end
%MATLAB fzero
opt=optimset('TolX',eps,'Display','iter');
[xv,fv]=fzero(f,[1e-4,0.1],opt)
