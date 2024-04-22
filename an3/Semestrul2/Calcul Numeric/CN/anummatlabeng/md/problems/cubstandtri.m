function Q=cubstandtri(F,tol,quadm,varargin)
%CUBSTANDTRI - cubature on standard triangle
%\int_0^1 \int_0^{1-x}f(x,y)dydx
%F - function
%tol - tolerance 
%quadm - method

if nargin < 1, error('Requires at least 1 argument'); end
if nargin < 2 | isempty(tol), tol = 1.e-6; end
if nargin < 3 | isempty(quadm), quadm = @quadl; end
F = fcnchk(F);
trace=[];
G = @(x,y) F(y,x);
Q = quadm(@innerint, 0, 1,  tol, trace,G, ...
              tol, quadm, varargin{:});

%---------
function Q = innerint(x, F, tol, quadm, varargin)
%INNERINT - used by  CUBSTANDTRI for inner integral
%
% quadm formula to be used
% Evaluates inner integral for each value of the
%  outer variable

Q = zeros(size(x)); trace=[];
for i = 1:length(x)
    Q(i) = quadm(F, 0, 1-x(i), tol, trace, x(i), varargin{:});
end