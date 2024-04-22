function Q = dblquadxy(intfcn,xmin,xmax,ymin,ymax,tol,quadf,varargin) 
%DBLQUAD Numerically evaluate double integral. 

if nargin < 5, error('MATLAB:dblquad:NotEnoughInputs',...
                     'Requires at least five inputs.'); end
if nargin < 6 || isempty(tol), tol = 1.e-6; end 
if nargin < 7 || isempty(quadf)
    quadf = @quad;
else
    quadf = fcnchk(quadf);
end
intfcn = fcnchk(intfcn);

trace = [];
G=@(x,y) intfcn(y,x);
Q = quadf(@innerintegral, xmin, xmax, tol, trace, G, ...
           ymin, ymax, tol, quadf, varargin{:}); 

%---------------------------------------------------------------------------

function Q = innerintegral(x, intfcn, ymin, ymax, tol, quadf, varargin) 
%INNERINTEGRAL Used with DBLQUAD to evaluate inner integral.
%
%   Q = INNERINTEGRAL(Y,INTFCN,XMIN,XMAX,TOL,QUADF)
%   Y is the value(s) of the outer variable at which evaluation is
%   desired, passed directly by QUAD. INTFCN is the name of the
%   integrand function, passed indirectly from DBLQUAD. XMIN and XMAX
%   are the integration limits for the inner variable, passed indirectly
%   from DBLQUAD. TOL is passed to QUAD (QUADL) when evaluating the inner 
%   loop, passed indirectly from DBLQUAD. The function handle QUADF
%   determines what quadrature function is used, such as QUAD, QUADL
%   or some user-defined function.

% Evaluate the inner integral at each value of the outer variable. 

%fcl = intfcn(ymin, x(1), varargin{:}); %evaluate only to get the class below
Q = zeros(size(x)); %, superiorfloat(fcl, ymax, x)); 
trace = [];
for i = 1:length(x) 
    Q(i) = quadf(intfcn, ymin, ymax, tol, trace, x(i), varargin{:}); 
end 
