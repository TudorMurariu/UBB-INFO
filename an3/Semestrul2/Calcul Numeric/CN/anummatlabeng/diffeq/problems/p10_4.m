function p10_4(N,gama)
%P10_4 - epidemia de gripa
%N - numarul de indivizi
%gama - rata de recontaminare

alfa=0.05; beta=0.0002;
tspan=[0,1500];
y0=[980,20,0];
opts=odeset('Events',@evf);
[t,w,te,ye,ie]=ode45(@gripa,tspan,y0,opts,alfa,beta,gama,N);
plot(t,w)
legend('susceptibili','infectati','imuni')
[ym,it]=max(w(:,2));
fprintf('numarul maxim de infectati :%f\n',ym)
fprintf('apare la momentul: %f\n',t(it))

function dydt=gripa(x,y,alfa,beta,gama,N)
dydt=[-beta*y(1)*y(2)+gama; beta*y(1)*y(2)-alfa*y(2);...
    alfa*y(2)-gama];
function [val,isterm,dir]=evf(t,y,alfa,beta,gama,N)
v=y(2)-0.9*N;
if v<=0
    val=1;
else
    val=0;
end
isterm=1; %eveniment terminal
dir=-1;   %descrescatoare