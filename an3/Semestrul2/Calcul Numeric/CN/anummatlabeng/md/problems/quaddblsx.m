function Q=quaddblsx(F,xmin,xmax,c,d,tol,quadm,varargin)
%QUADDBLSX - cuadratura dubla 
%\int_a^b \int_c(x)^d(x)f(x,y)dxdy
%domeniu simplu in raport cu x
%F - functia
%xmin,xmax - limite dupa x
%c,d - limitele dupa y (functii de x)
%tol - precizia
%quadm - metoda

if nargin < 5, error('Necesita minim 5 argumente'); end
if nargin < 6 | isempty(tol), tol = 1.e-6; end
if nargin < 7 | isempty(quadm), quadm = @quad; end
F = fcnchk(F);
trace=[];
G = @(x,y) F(y,x);
Q = quadm(@innerint, xmin, xmax,  tol, trace,G, ...
              c,d,  tol, quadm, varargin{:});

%---------
function Q = innerint(x, F, c, d, tol, quadm, varargin)
%INNERINT - utilizata de QUADDBL pentru integrala interioara.
%
% quadm determina formula de cuadratura ce va fi utilizata
% Evalueaza integrala interioara pentru fiecare valoare
%  a variabilei exterioare

Q = zeros(size(x)); trace=[];
for i = 1:length(x)
    li=c(x(i)); 
    ls=d(x(i));
    Q(i) = quadm(F, c(x(i)), d(x(i)), tol, trace, x(i), varargin{:});
end