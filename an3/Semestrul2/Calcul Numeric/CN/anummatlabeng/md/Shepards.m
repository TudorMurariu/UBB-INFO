function z=Shepards(X,P,f,mu)
%SHEPARDS simple n-variate Shepard interpolant at one point
%call Z=SHEPARDS(X,P,F,MU)
%Parameters
%X - the points
%P - interpolation nodes
%F - function values at points P 
%MU - the exponent

[dim,np]=size(P);
[dx,nx]=size(X);
dxp=dist(X,P);
z=zeros(1,nx);
s=z;
for j=1:np
    index=[1:j-1,j+1:np];
    p=prod(dxp(index,:)).^mu;
    z=z+p*f(j);
    s=s+p;
end
z=z./s;