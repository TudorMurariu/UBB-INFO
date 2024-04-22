function fluture(a,b,c,lp)
if nargin<4, lp=1; end
t=linspace(0,2*lp*pi,600);
r=exp(cos(t))-a*cos(b*t)+(sin(c*t)).^5;
polar(t,r)