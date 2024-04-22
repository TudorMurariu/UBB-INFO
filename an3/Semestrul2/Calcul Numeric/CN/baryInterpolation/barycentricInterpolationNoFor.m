function ff = barycentricInterpolationNoFor(x, y, xx, c)
    %BARYCENTRICINTERPOLATION - barycentric Lagrange interpolation
    %call ff=barycentricInterpolation(x,y,xx,c)
    %x -  nodes
    %y - function values
    %xx - interpolation points
    %c - barycentric weights 
    %ff - values of interpolation polynomial

n=length(x)-1;
numer = zeros(size(xx));
denom = zeros(size(xx));
exact = zeros(size(xx)); %test if xx=nodes

% difference between target point and input points
xdiff = xx - x;
% weights for Lagrange interpolation formula
temp = c ./ xdiff;
% numerator of Lagrange interpolation formula
numer = sum(temp .* y);
% denominator of Lagrange interpolation formula
denom = sum(temp);
% indices of exact matches between target point and input points
exact = find(xdiff == 0);

ff = numer ./ denom;
jj = find(exact); 
ff(jj) = y(exact(jj));
end
