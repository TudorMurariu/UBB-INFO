function [Q,fcount] = adquad(F,a,b,tol,varargin)
%ADQUAD  adaptive quadrature
%call [Q,fcount] = adquad(F,a,b,tol,varargin)
% F - integrand
% a,b - interval endpoints 
% tol - tolerance, default 1.e-6.
% the aditional arguments are passed to the integrand, F(x,p1,p2,..).

% make F callable by feval.
if ischar(F) && exist(F)~=2
   F = inline(F);
elseif isa(F,'sym')
   F = inline(char(F));
end 

if nargin < 4 || isempty(tol), tol = 1.e-6; end

% Initialization 
c = (a + b)/2;
fa = F(a,varargin{:}); fc = F(c,varargin{:});
fb = F(b,varargin{:});

% Recursive call
[Q,k] = quadstep(F, a, b, tol, fa, fc, fb, varargin{:});
fcount = k + 3;

% ---------------------------------------------------------

function [Q,fcount] = quadstep(F,a,b,tol,fa,fc,fb,varargin)

% Recursive subfunction called by adquad

h = b - a; 
c = (a + b)/2;
fd = F((a+c)/2,varargin{:});
fe = F((c+b)/2,varargin{:});
Q1 = h/6 * (fa + 4*fc + fb);
Q2 = h/12 * (fa + 4*fd + 2*fc + 4*fe + fb);
if abs(Q2 - Q1) <= tol
   Q  = Q2 + (Q2 - Q1)/15;
   fcount = 2;
else
   [Qa,ka] = quadstep(F, a, c, tol, fa, fd, fc, varargin{:});
   [Qb,kb] = quadstep(F, c, b, tol, fc, fe, fb, varargin{:});
   Q  = Qa + Qb;
   fcount = ka + kb + 2;
end
