function [lx,ly]=subdivision(x,y,niv)
%P1_10 recursive subdivision of a triangle
%over midpoints
%x,y - vertex coordinates
%lx,ly - point coordinates (output)
%niv - subdivision level

global LP
P=[x(:),y(:)];
LP=P;
if niv>0
    subdr(P,niv);
end
lx=LP(:,1); ly=LP(:,2);

function subdr(P,niv)
global LP
%recursive subdivision
m1=(P(1,:)+P(2,:))/2;
m2=(P(2,:)+P(3,:))/2;
m3=(P(1,:)+P(3,:))/2;
LP=[LP;m1;m2;m3];
if niv>1
    subdr([P(1,:);m1;m3],niv-1);
    subdr([P(2,:);m2;m1],niv-1);
    subdr([P(3,:);m3;m2],niv-1);
    subdr([m1;m2;m3],niv-1);
end
