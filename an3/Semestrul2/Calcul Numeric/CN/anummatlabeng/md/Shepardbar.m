function z=Shepardbar(X,P,f,mu)
%SHEPARDBAR computes the simple n-variate Shepard interpolant
%barycentric form
%call Z=SHEPARDBAR(X,P,F,MU)
%Parameters
%X - the points
%P - interpolation nodes
%F - function values at points P 
%MU - the exponent

[dim,np]=size(P);
[dx,nx]=size(X);
dxp=(dist(X,P)).^(-mu);
z=f*dxp./sum(dxp);