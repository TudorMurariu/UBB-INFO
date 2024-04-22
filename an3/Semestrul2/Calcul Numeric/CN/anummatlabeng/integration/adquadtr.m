function [Q,fcount] = adquadtr(F,a,b,tol,tr,varargin)
%ADQUAD  Cuadratura adaptiva
%apel [Q,fcount] = adquad(F,a,b,tol,varargin)
% F - functia
% a,b - intervalul
% tol precizia, inplicit 1.e-6.
% restul argumentelor se transmit integrandului , F(x,p1,p2,..).

% Face  F apelabila prin  feval.

global X Y
if ischar(F) & exist(F)~=2
    F = inline(F);
elseif isa(F,'sym')
    F = inline(char(F));
end 

if nargin < 4 | isempty(tol), tol = 1.e-6; end
if nargin < 5 | isempty(tr), tr=0; end


% Initializare 
c = (a + b)/2;
fa = fevaltr(F,a,tr,varargin{:}); fc = fevaltr(F,c,tr,varargin{:});
fb = fevaltr(F,b,tr,varargin{:});

% Apel recursiv
X=[]; Y=[];
[Q,k] = quadstep(F, a, b, tol, fa, fc, fb, tr, varargin{:});
fcount = k + 3;
if tr,clf 
    [X,ii]=sort(X); Y=Y(ii); 
    plot(X,Y,'-o');  hold on %plot(X,zeros(size(X)),'k+');
    set(gca,'Xtick',X); 
    set(gca,'XTickLabel',[])
    title(['Value of integral=',num2str(Q)])
    p=get(gca,'Position');
    hb=axes('Position',[p(1),p(2)-0.002,p(3),0.002]);
    set(hb,'Xtick',linspace(0,1,7))
    ddiv=linspace(a,b,7);
    ss=sprintf('%5.2f|',ddiv(1:end-1));
    ss=[ss,sprintf('%5.2f',ddiv(end))];
%    set(hb,'XTickLabel','-1|-0.5|0|0.5|1|1.5|2')
    set(hb,'XTickLabel',ss)
    hold off
end

% ---------------------------------------------------------

function [Q,fcount] = quadstep(F,a,b,tol,fa,fc,fb,tr,varargin)

% Subfunctie recursiva utilizata de adquad.

h = b - a; 
c = (a + b)/2;
fd = fevaltr(F,(a+c)/2,tr,varargin{:});
fe = fevaltr(F,(c+b)/2,tr,varargin{:});
Q1 = h/6 * (fa + 4*fc + fb);
Q2 = h/12 * (fa + 4*fd + 2*fc + 4*fe + fb);
if abs(Q2 - Q1) <= tol
    Q  = Q2 + (Q2 - Q1)/15;
    fcount = 2;
else
    [Qa,ka] = quadstep(F, a, c, tol, fa, fd, fc, tr, varargin{:});
    [Qb,kb] = quadstep(F, c, b, tol, fc, fe, fb, tr, varargin{:});
    Q  = Qa + Qb;
    fcount = ka + kb + 2;
end

%---------
function y=fevaltr(f,x,tr,varargin)
global X Y
y=feval(f,x,varargin{:});
if tr
    X=[X;x(:)]; Y=[Y;y(:)]; 
end