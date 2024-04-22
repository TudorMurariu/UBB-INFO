function drawbezier(pc,npc,cul)
%deseneaza o curba Bezier
%drawbezier(pc,npc)
%pc - punctele de control
%npc - numar de puncte (rezolutia)
h=1/npc;
t=0:h:1;
uu=deCasteljau(pc,t);
plot(pc(1,:),pc(2,:),'--',pc(1,:),pc(2,:),'o',uu(1,:),uu(2,:),cul);