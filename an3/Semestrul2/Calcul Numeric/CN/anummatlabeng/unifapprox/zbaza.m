kn=-5:10;
t=-1:0.025:6;
clf
for k=1:10
    pc=[zeros(1,k),1,zeros(1,16-k-1)];
    q=Cox_deBoor(3,pc,kn,t);
    plot(t,q);
    hold on
end