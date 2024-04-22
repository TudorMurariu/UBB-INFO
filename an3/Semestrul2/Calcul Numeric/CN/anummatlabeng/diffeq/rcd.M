function rcd
%RCD Stiff ODE for reaction-convection-diffusion problem 
% obtained from method of lines

N = 40; a = 1; b = 5e-2;
tspan = [0;2]; space = [1:N]/(N+1);

y0 = 0.5*(1+cos(2*pi*space));
y0 = y0(:);
options = odeset('Jacobian',@jacobian,'Jpattern',...
    jpattern(N),'RelTol',1e-3,'AbsTol',1e-3);

[t,y] = ode15s(@f,tspan,y0,options,N,a,b);
e = ones(size(t)); U = [e y e];
waterfall([0:1/(N+1):1],t,U)
xlabel('space','FontSize',16,'Interpreter','LaTeX') 
ylabel('time','FontSize',16,'Interpreter','LaTeX')

% ---------------------------------------------------------
% Subfunctions
% ---------------------------------------------------------
function dydt = f(t,y,N,a,b)
%F         Differential equation

r1 = -a*(N+1)/2;
r2 = b*(N+1)^2;
up = [y(2:N);0]; down = [0;y(1:N-1)];
e1 = [1;zeros(N-1,1)]; eN = [zeros(N-1,1);1];

dydt = r1*(up-down) + r2*(-2*y+up+down) + (r2-r1)*e1 +... 
    (r2+r1)*eN + y.*(1-y);

% ---------------------------------------------------------
function dfdy = jacobian(t,y,N,a,b)
%JACOBIAN  Jacobian matrix

r1 = -a*(N+1)/2;
r2 = b*(N+1)^2;
u = (r2-r1)*ones(N,1);
v = (-2*r2+1)*ones(N,1) - 2*y;
w = (r2+r1)*ones(N,1);

dfdy = spdiags([u v w],[-1 0 1],N,N);

% ---------------------------------------------------------
function S = jpattern(N)
%JPATTERN  Sparsity pattern of Jacobian matrix

e = ones(N,1);
S = spdiags([e e e],[-1 0 1],N,N);
