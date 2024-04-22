function yp = r3body(t,y)
% R3BODY  Function defining the restricted three-body ODEs

M = 0.012277471;
E = 1 - M;
%
r1 = sqrt((y(1)+M)^2 + y(3)^2);
r2 = sqrt((y(1)-E)^2 + y(3)^2);
%
yp2 = 2*y(4) + y(1) - E*(y(1)+M)/r1^3 - M*(y(1)-E)/r2^3;
yp4 = -2*y(2) + y(3) - E*y(3)/r1^3 - M*y(3)/r2^3;
yp = [y(2); yp2; y(4); yp4];