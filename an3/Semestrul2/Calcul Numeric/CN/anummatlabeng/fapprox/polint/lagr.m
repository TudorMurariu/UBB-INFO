function fi=lagr(x,y,xi)
%LAGR - computes Lagrange interpolation polynomial
% x,y - coordinates of nodes 
% xi - evaluation points

if nargin ~=3 
    error('ilegal no. of arguments')
end
[mu,nu]=size(xi);
fi=zeros(mu,nu);
np1=length(y);
for i=1:np1
    z=ones(mu,nu);
    for j=[1:i-1,i+1:np1]
        z=z.*(xi-x(j))/(x(i)-x(j));
    end;
    fi=fi+z*y(i);
end
