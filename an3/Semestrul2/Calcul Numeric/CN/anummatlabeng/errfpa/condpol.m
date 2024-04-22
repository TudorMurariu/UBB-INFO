function nc=condpol(p,xi)
%CONDPOL - condition of the root of an algebraic equation
%call NC=CONDPOL(P,XI)

if nargin<2
    xi=roots(p);
end
n=length(p)-1;
dp=polyder(p); %derivative;
nc=1./(abs(xi.*polyval(dp,xi))).*(polyval(abs(p(2:end)),abs(xi)));
