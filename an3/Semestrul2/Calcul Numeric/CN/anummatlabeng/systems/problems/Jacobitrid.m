function x=Jacobitrid(A,b,era,err,Nmax)
%JACOBITRID - metoda lui Jacobi pentru sistem tridiagonal

[m,n]=size(A);
if nargin<5, Nmax=50; end
if nargin<4, err=0; end
if nargin<3, era=1e-3; end
xp=zeros(n,1); xc=xp;
for k=1:Nmax
    xc(1)=1/A(1,1)*(b(1)-A(1,2)*xp(2));
    for i=2:n-1    
        xc(i)=1/A(i,i)*(b(i)-A(i,i-1)*xp(i-1)-A(i,i+1)*xp(i+1));
    end
    xc(n)=1/A(n,n)*(b(n)-A(n,n-1)*xp(n-1));
    if norm(xc-xp,inf)<era+err*norm(xc)
        x=xc;
        return;
    end
    xp=xc;
end
error('prea multe iteratii')
