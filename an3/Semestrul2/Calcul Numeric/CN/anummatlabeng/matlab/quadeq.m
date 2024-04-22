function x = quadeq(a,b,c)
% QUADEQ Find roots of a quadratic equation.
%    X = QUADRATIC(A,B,C) returns the two roots of 
%    y = A*x^2 + B*x + C.
%    The roots are contained in X = [X1 X2].

denom = 2*a(:);
delta = discr(a,b,c);  % Root of the discriminant
x1 = (-b + delta)./denom;
x2 = (-b - delta)./denom;
x = [x1(:), x2(:)];
end
function d = discr(a,b,c)
d = sqrt(b().^2-4*a(:).*c(:));
end