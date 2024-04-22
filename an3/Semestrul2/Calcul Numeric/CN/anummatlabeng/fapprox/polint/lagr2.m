function fi=lagr2(x,y,xi)
%LAGR2 - computes Lagrange interpolation polynomial
%        using basic polynomials
% x,y - node coordinates 
% xi - evaluation points

if nargin ~=3 
error('wrong number of arguments')
end
z=pfl(x,xi);
fi=y*z;
