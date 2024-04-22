function p=polyperim(x,y)
%POLYPERIM - computes the perimeter of a polygon
%x,y - vertex coordinates (given in order)
x=x(:); y=y(:); x=[x;x(1)]; y=[y;y(1)];
p=sum((diff(x).^2+diff(y).^2).^(1/2));