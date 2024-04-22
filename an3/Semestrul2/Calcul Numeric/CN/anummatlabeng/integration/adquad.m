function [Q,fcount] = adquad(F,a,b,tol,tr,varargin)
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

if nargin < 5 || isempty(tr), tr=0; end
if nargin < 4 || isempty(tol), tol = 1.e-6; end

% Initialization 
c = (a + b)/2;
fa = feval(F,a,varargin{:}); fc = feval(F,c,varargin{:});
fb = feval(F,b,varargin{:});
if tr, [a, fa; b, fb; c, fc], end
% Recursive call
[Q,k] = quadstep(F, a, b, tol, tr,  fa, fc, fb, varargin{:});
fcount = k + 3;

% ---------------------------------------------------------

function [Q,fcount] = quadstep(F,a,b,tol,tr,fa,fc,fb,varargin)

% Recursive subfunction called by adquad

h = b - a; 
c = (a + b)/2;
fd = feval(F,(a+c)/2,varargin{:});
fe = feval(F,(c+b)/2,varargin{:});
if tr, [(a+c)/2, fd; (c+b)/2, fe], end
Q1 = h/6 * (fa + 4*fc + fb);
Q2 = h/12 * (fa + 4*fd + 2*fc + 4*fe + fb);
if abs(Q2 - Q1) <= tol
   Q  = Q2 + (Q2 - Q1)/15;
   fcount = 2;
else
   [Qa,ka] = quadstep(F, a, c, tol, tr, fa, fd, fc, varargin{:});
   [Qb,kb] = quadstep(F, c, b, tol, tr, fc, fe, fb, varargin{:});
   Q  = Qa + Qb;
   fcount = ka + kb + 2;
end
