function fi=Lagrange(x,y,xi)
%LAGRANGE - compute Lagrange interpolation polynomial
%call fi=Lagrange(x,y,xi)
% x,y - nodes coordinates
% xi - evaluation points

if nargin ~=3 
    error('Illegal number of arguments')
end
[mu,nu]=size(xi);
fi=zeros(mu,nu);
np1=length(y);
for i=1:np1
    z=ones(mu,nu);
    for j=[1:i-1,i+1:np1]
        z=z.*(xi-x(j))/(x(i)-x(j));
    end
    fi=fi+z*y(i);
end
