%P6_4
pc=[1,-1,1,-1;0,1,1,0]; npc=200;
pc2=pc; pc3=pc;
drawbezier(pc,npc,'m')
hold on
pcd=(diff(pc'))';
v=deCasteljau(pcd,1/2)
pc2(1,2)=pc(1,2)-0.2;
pc2(1,3)=pc(1,3)+0.2;
drawbezier(pc2,npc,'r')
pc3(1,2)=pc(1,2)+0.2;
pc3(1,3)=pc(1,3)-0.2;
drawbezier(pc3,npc,'g')
