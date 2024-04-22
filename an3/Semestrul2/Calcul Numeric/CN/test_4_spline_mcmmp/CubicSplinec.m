function c=CubicSplinec(x,f,type,der)
%CUBICSPLINE - find coefficients of a cubic spline
%call c=Splinecubic(x,f,type,der)
%x - abscissas 
%f - ordinates 
%type - 0  complete (clamped)
%       1  match second derivatives
%       2  natural
%       3  not a knot (deBoor)
%der - values of derivatives
%      [f'(a),f'(b)] for type 0
%      [f''(a), f''(b)] for type 1
%c - coefficients (length(x)-1) by 4 matrix, in decreasing order/line
%Method - see Gautschi - Numerical Analysis. An Introduction. 2nd edition
% Birkhauser

if (nargin<4) || (type==2), der=[0,0]; end %natural spline

n=length(x);
if any(diff(x)<0), [x,ind]=sort(x); else ind=1:n; end %order nodes if needed
y=f(ind); x=x(:); y=y(:);
%auxiliary unknowns - values of the spline derivative
dx=diff(x);  %compute delta_x
ddiv=diff(y)./dx; %divided differences
ds=dx(1:end-1); dd=dx(2:end); %prepare diagonals
dp=2*(ds+dd);                 
md=3*(dd.*ddiv(1:end-1)+ds.*ddiv(2:end)); %rhs
switch type
case 0 %complete
    dp1=1; dpn=1; vd1=0; vdn=0;
    md1=der(1); mdn=der(2);
case {1,2}
    dp1=2; dpn=2; vd1=1; vdn=1;
    md1=3*ddiv(1)-0.5*dx(1)*der(1);
    mdn=3*ddiv(end)+0.5*dx(end)*der(2);
case 3 %deBoor
    x31=x(3)-x(1);xn=x(n)-x(n-2);
    dp1=dx(2); dpn=dx(end-1);
    vd1=x31;
    vdn=xn;
    md1=((dx(1)+2*x31)*dx(2)*ddiv(1)+dx(1)^2*ddiv(2))/x31;
    mdn=(dx(end)^2*ddiv(end-1)+(2*xn+dx(end))*dx(end-1)*ddiv(end))/xn;
end
%sparse system
dp=[dp1;dp;dpn];
dp1=[0;vd1;dd];
dm1=[ds;vdn;0];
md=[md1;md;mdn];
A=spdiags([dm1,dp,dp1],-1:1,n,n);
m=A\md;
c(:,4)=y(1:end-1);
c(:,3)=m(1:end-1);
c(:,1)=(m(2:end)+m(1:end-1)-2*ddiv)./(dx.^2);
c(:,2)=(ddiv-m(1:end-1))./dx-dx.*c(:,1);

