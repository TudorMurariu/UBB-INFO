function polarrosy(a,b)

if nargin<2, b=1; end
if nargin<1, a=1; end
c=0; d=1; p=a*b;
[t,r]=spiro(a,a,c,d,p,1/2);
polar(a*t,r)
s=sprintf('rosy(%d,%d)',a,b);
title(s)
end
function [t,r]=spiro(a,b,c,d,p,k)
h=k*2*pi/180;
t=(0:h:2*pi)';
r=c+d*sin(t*p);
end
