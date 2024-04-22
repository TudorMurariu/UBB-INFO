%SEWINGMACHINE2

load sm
s=zeros(length(sm),1);
s(2:end)=abs(diff(sm(:,1)))+abs(diff(sm(:,2)));
s=cumsum(s);
t=linspace(s(1),s(end),300);
xg=spline(s,sm(:,1),t);
yg=spline(s,sm(:,2),t);
plot(sm(:,1),sm(:,2),'o',xg,yg)
