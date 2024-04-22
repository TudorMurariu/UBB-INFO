function pc=cpinput(xmin,xmax,ymin,ymax)
%apel [u,v]=cpinput(xmin,xmax,ymin,ymax) sau [u,v]=cpinput
%daca se apeleaza cu 4 argumente da limitele de fereastra
if nargin==4
   axis([xmin,xmax,ymin,ymax]);
end
hold on
u=[];
v=[];
[x,y,b]=ginput(1);
while b~=3
   u=[u;x];
   v=[v;y];
   plot(u,v,'-',u,v,'o')
   [x,y,b]=ginput(1);
end;
pc=[u';v'];
hold off
