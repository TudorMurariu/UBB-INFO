%GAS
alpha=188.33;
beta_p=9.77e-4;
P=1013250;
R=8.3144721515/0.044;
%R=11.0163766063776
T=300;
f = @(v) (P+alpha./v.^2).*(v-beta_p)-R*T;
v=linspace(1e-3,0.1,5000);
plot(v,f(v))
vm=0.056;
[xv,fv]=fzero(f,[0.02,0.1])
for x0=[1e-4,1e-3,1e-2,1e-1]
    [xv,nit]=Newton(f,x0,0,eps,200)
end