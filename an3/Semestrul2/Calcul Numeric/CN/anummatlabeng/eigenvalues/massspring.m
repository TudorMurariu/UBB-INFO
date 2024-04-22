function [T,Locations,Velocities]=massspring(n,m,b,k,x0,x,v,dt,tfinal)
% MASSSPRING Solve vibrating mass-spring system using eigenvalues
%
% Inputs
%   N = number of bodies
%   M = column vector of n masses
%   B = column vector of n damping constants
%   K = column vector of n spring constants
%   X0= column vector of n rest positions of bodies
%   X = column vector of n initial displacements from rest
%   V = column vector of n initial velocities of bodies
%   DT = time step
%   TFINAL = final time to integrate system
%
% Outputs
%   graph body positions, T, Locations, Velocities
%

% Compute mass matrix
M = diag(m);
% Compute damping matrix
B = diag(b);
% Compute stiffness matrix
if n>1,
    K = diag(k) + diag([k(2:n);k(n)]) - diag(k(2:n),1) - diag(k(2:n),-1);
else
    K = k;
end
% Compute matrix governing motion, find its eigenvalues and eigenvectors
A = [[-inv(M)*B, -inv(M)*K];[eye(n),zeros(n)]];
[V,D]=eig(A);
dD = diag(D);
iV = inv(V);
i=1;
Y = [v;x];
T = 0;
steps = round(tfinal/dt);
% Compute positions and velocities
for i = 2:steps
    t = (i-1)*dt;
    T = [T,t];
    Y(1:2*n,i) = V * ( diag(exp(t*dD)) * ( iV * [v;x] ) );
end
Y = real(Y);
hold off, clf
subplot(2,1,1)
Locations = Y(n+1:2*n,:)+x0*ones(1,i);
attr={'k-','r--','g-.','b:'};
for j=1:n,
    color=attr{rem(j,4)+1};
    plot(T,Locations(j,:),color),
    hold on
    axis([0,tfinal,min(min(Locations)),max(max(Locations))])
end
title('Positions')
xlabel('Time')
grid
subplot(2,1,2)
Velocities = Y(1:n,:);
for j=1:n,
    color=attr{rem(j,4)+1};
    plot(T,Velocities(j,:),color),
    hold on
    axis([0,tfinal,min(min(Velocities)),max(max(Velocities))])
end
title('Velocities')
xlabel('Time')
grid
