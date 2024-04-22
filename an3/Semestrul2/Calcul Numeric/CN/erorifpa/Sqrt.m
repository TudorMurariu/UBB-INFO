function y=Sqrt(a)
% SQRT computes the square-root of a positive number
% y=Sqrt(a); computes the square-root of the positive real
% number a using Newton’s method, up to machine precision.
%% 
% Initialization
%
% $$\frac{a+1}{2}\geq \sqrt{a}$
%
xo=(1+a)/2; xn=(xo+a/xo)/2;
% stopping criterion breaking the monotony
while xn<xo
    xo=xn; xn=(xo+a/xo)/2;
end
y=(xo+xn)/2;
end

