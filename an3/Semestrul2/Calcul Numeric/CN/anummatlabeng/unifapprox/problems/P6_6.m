%P6_6
clf
n=input('n=');
t=linspace(0,6*pi,n);
g=3; r=1;
pc=[r*cos(t); r*sin(t); t];
n=length(pc); npd=200;
kn=noduri(n-1,g);
ln=length(kn); 
pas=kn(ln)/npd;
t=0:pas:kn(ln);
q=Cox_deBoor(g,pc,kn,t);
plot3(pc(1,:),pc(2,:),pc(3,:),'--o');
hold on
plot3(q(1,:),q(2,:),q(3,:),'Linewidth',2,'Color','r')