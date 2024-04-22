function paramrosy(a,b)

if nargin<2, b=1; end
if nargin<1, a=1; end
c=0; d=1; p=a*b;
[x,y]=spiro(a,a,c,d,p,1/2);
plot(x,y)
s=sprintf('rosy(%d,%d)',a,b);
title(s)
axis square, axis off
end
function [x,y]=spiro(a,b,c,d,p,k)
h=k*2*pi/180;
t=(0:h:2*pi)';
r=c+d*sin(t*p);
x=r.*cos(a*t);
y=r.*sin(b*t);
end
