function [tout,yout] = oderk(F,tspan,y0,BT,opts,varargin)
%ODERK nonstiff ODE solver
%   ODERK uses two embedded methods given by Butcher table
%
%   ODERK(F,TSPAN,Y0) with TSPAN = [T0 TFINAL] integrates the sistem 
%   of differential equations y' = f(t,y) from t = T0 to t = TFINAL. 
%   Initial condition is y(T0) = Y0.  F is an M-file name, an inline
%   function or a character string definening f(t,y).  
%   This function must have two arguments, t and y and must return
%   a column vector of derivatives, yprime.
%
%   With two output arguments, [T,Y] = ODERK(...) return a column vector 
%   T and an array Y, where Y(:,k) is the solution at point T(k).
%
%   Without output arguments, ODERK plots the solution.
%
%   ODERK(F,TSPAN,Y0,RTOL) uses the relative error RTOL, instead of 
%   default 1.e-3.
%
%   ODERK(F,TSPAN,Y0,BT) uses a Butcher table, BT. If BT is empty or 
%   missing, one uses BS23 (Bogacki-Shampine)
%
%   ODERK(F,TSPAN,Y0,BT,OPTS) where OPTS = ODESET('reltol',RTOL, ...
%   'abstol',ATOL,'outputfcn',@PLOTFUN) uses the relative error
%   RTOL instead the default 1.e-3, the absolute error ATOL instead of 
%   1.e-6 and call PLOTFUN instead of ODEPLOT after each successful step
%
%   If the call has more than 5 input arguments,
%   ODERK(F,TSPAN,Y0,BT,RTOL,P1,P2,...), the adittional arguments are 
%   passed to F, F(T,Y,P1,P2,...).
%
%   Stats set to 'on' provides statistics
%
%   Example
%      tspan = [0 2*pi];
%      y0 = [1 0]';
%      F = '[0 1; -1 0]*y';
%      oderk(F,tspan,y0);

% Initialize variables

rtol = 1.e-3;
atol = 1.e-6;
plotfun = @odeplot;
statflag=0;

if (nargin >= 4) & ~isempty(BT)
    [lambda,alfa,alfas,mu,s,oop,fsal]=BT(); %Butcher table
else
    [lambda,alfa,alfas,mu,s,oop,fsal]=BS23(); %Bogacki-Shampine
end

if nargin >= 5 & isnumeric(opts)
    rtol = opts;
elseif nargin >= 5 & isstruct(opts)
    statflag=strcmp(opts.Stats,'on');
    if ~isempty(opts.RelTol), rtol = opts.RelTol; end
    if ~isempty(opts.AbsTol), atol = opts.AbsTol; end
    if ~isempty(opts.OutputFcn), plotfun = opts.OutputFcn; end
end
if statflag  %statistics
    stat=struct('ns',0,'nrej',0,'nfunc',0);
end

t0 = tspan(1);
tfinal = tspan(2);
tdir = sign(tfinal - t0);
plotit = (nargout == 0);
threshold = atol / rtol;
hmax = abs(0.1*(tfinal-t0));
t = t0;
y = y0(:);

% Make F callable

if ischar(F) & exist(F)~=2
    F = inline(F,'t','y');
elseif isa(F,'sym')
    F = inline(char(F),'t','y');
end 

% Init outputs

if plotit
    plotfun(tspan,y,'init');
else
    tout = t;
    yout = y.';
end

% Compute initial stepsize

K=zeros(length(y0),s);
K(:,1)=F(t,y,varargin{:});  %first evaluation
if statflag, stat.nfunc=stat.nfunc+1; end
r = norm(K(:,1)./max(abs(y),threshold),inf) + realmin;
h = tdir*0.8*rtol^(oop)/r;

% main loop

while t ~= tfinal
    
    hmin = 16*eps*abs(t);
    if abs(h) > hmax, h = tdir*hmax; end
    if abs(h) < hmin, h = tdir*hmin; end
    
    % corect final stepsize
    
    if 1.1*abs(h) >= abs(tfinal - t)
        h = tfinal - t;
    end
    
    % compute step attempt
    
    for i=2:s
        K(:,i)=F(t+mu(i)*h,y+h*K(:,1:i-1)*(lambda(i,1:i-1)'));
    end
    if statflag, stat.nfunc=stat.nfunc+s-1; end
    tnew=t+h;
    ynew=y+h*K*alfas;

    % Estimate error
    
    e = h*K*(alfa-alfas);
    err = norm(e./max(max(abs(y),abs(ynew)),threshold),inf) + realmin;
    
    % Accept sulutions if estimated error < tolerance
    
    if err <= rtol %successful step
        t = tnew;
        y = ynew;
        if plotit
            if plotfun(t,y,'');
                break
            end
        else
            tout(end+1,1) = t;
            yout(end+1,:) = y.';
            if statflag
                stat.ns=stat.ns+1;
            end
        end
        if fsal % Reuse final value if needed
            K(:,1)=K(:,s);
        else
            K(:,1)=F(t,y);
            if statflag, stat.nfunc=stat.nfunc+1; end
        end
    else %rejected step
        if statflag, stat.nrej=stat.nrej+1; end
    end
    
    % compute new step 
    
    h = h*min(5,0.8*(rtol/err)^(oop));
    
    % Exit if stepsize too small
    
    if abs(h) <= hmin
        warning(sprintf('step size %e too small at t = %e.\n',h,t));
        t = tfinal;
    end
end

if plotit
    plotfun([],[],'done');
end
if statflag
    fprintf('%d succesfull steps\n',stat.ns)
    fprintf('%d failed attempts\n', stat.nrej)
    fprintf('%d function evaluations\n', stat.nfunc)
end
