function P5_7(P1,P2,R1,R2)
%P5_7 cubica Hermite
t=linspace(0,1,200);
x=[0,1];
[z,td]=difdivnd(x,[P1(1),P2(1)],[R1(1),R2(1)]);
xg=pNewton(td,z,t);
[z2,td2]=difdivnd(x,[P1(2),P2(2)],[R1(2),R2(2)]);
yg=pNewton(td2,z2,t);
V1=P1+R1; V2=P2+R2;
plot(xg,yg,[P1(1),V1(1)],[P1(2),V1(2)],'r-',...
    V1(1),V1(2),'r>',...
[P2(1),V2(1)],[P2(2),V2(2)],'r-',...
V2(1),V2(2),'r<')