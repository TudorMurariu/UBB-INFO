function A=polyarea(x,y)
%POLYPERIM - computes the area of a polygon
%x,y - vertex coordinates (given in order)
x=x(:); y=y(:); x=[x;x(1)]; y=[y;y(1)];
A=1/2*abs(sum(x(1:end-1).*y(2:end)-x(2:end).*y(1:end-1)));