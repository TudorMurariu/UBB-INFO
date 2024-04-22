function Q = quaddbl(F,xmin,xmax,ymin,ymax,tol,...
    quadm,varargin)
%QUADDBL - approximates a double integral on a rectangle
%Parameters
%F - Integrand
%XMIN, XMAX, YMIN, YMAX - rectangle limits
%TOL -tolerance, default 1e-6
%QUADM - integration method,  default adquad
if nargin < 5, error('Required at lest 5 arguments'); end
if nargin < 6 || isempty(tol), tol = 1.e-6; end
if nargin < 7 || isempty(quadm), quadm = @adquad; end
F = fcnchk(F);

Q = quadm(@innerint, ymin, ymax, tol, [], F, ...
           xmin, xmax, tol, quadm, varargin{:});

%---------
function Q = innerint(y, F, xmin, xmax, tol, quadm, varargin)
%INNERINT - used by QUADDBL for inner integral.
%
% QUADM specifies quadrature to be used
% Evaluates inner integral for each value of outer variable

Q = zeros(size(y));
for i = 1:length(y)
    Q(i) = quadm(F, xmin, xmax, tol, [], y(i), varargin{:});
end
