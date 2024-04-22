function [w,wex,modes,x,y,nx,ny,ax,by]=recmemfr(...
    ax,by,nx,ny,noplt)
%
% [w,wex,modes,x,y,nx,ny,ax,by]=recmemfr(a,b,nx,ny,noplt)
% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
% This function employs finite difference methods to
% estimate the natural frequencies and mode shapes of
% a rectangular membrane having fixed edges.
% ax, by - membrane side lengths along the x and y axes
% nx,ny - number of finite difference points taken in
% the x and y directions including the edges
% w - vector of (nx-2)*(ny-2) frequencies obtained
% by finite difference approximation of the
% wave equation. These are arranged in
% increasing order
% wex - vector of exact frequencies
% modes - three dimensional array containing the mode
% shapes for various frequencies. The array
% size is [nx,ny,(nx-2)*(nx-2)] denoting
% the x direction, y direction, and the
% freqency numbers matching components of the
% w vector. The i’th mode shape is obtained
% as reshape(vecs(:,i),n,m)
% x,y - vectors defining the finite difference grid
% noplt - optional parameter included if no plot of
% the approximate and exact frequencies is to
% be made

if nargin==0; ax=2; nx=20; by=1; ny=10; end
dx=ax/(nx-1); dy=by/(ny-1);
na=(1:nx-1)'/ax; nb=(1:ny-1)/by;

% Compute exact frequencies for comparison
wex=pi*sqrt(repmat(na.^2,1,ny-1)+repmat(nb.^2,nx-1,1));
wex=sort(wex(:)'); x=linspace(0,ax,nx);
y=linspace(0,by,ny); neig=(nx-2)*(ny-2); nvar=nx*ny;
% Form equations to fix membrane edges
k=0; s=[nx,ny]; c=zeros(2*(nx+ny),nvar);
for j=1:nx
    m=sub2ind(s,[j,j],[1,ny]); k=k+1;
    c(k,m(1))=1; k=k+1; c(k,m(2))=1;
end
for j=1:ny
    m=sub2ind(s,[1,nx],[j,j]); k=k+1;
    c(k,m(1))=1; k=k+1; c(k,m(2))=1;
end

% Form frequency equations at interior points
k=0; a=zeros(neig,nvar); b=a;
phi=(dx/dy)^2; psi=2*(1+phi);
for i=2:nx-1
    for j=2:ny-1
        m=sub2ind(s,[i-1,i,i+1,i,i],[j,j,j,j-1,j+1]);
        k=k+1; a(k,m(1))=-1; a(k,m(2))=psi; a(k,m(3))=-1;
        a(k,m(4))=-phi; a(k,m(5))=-phi; b(k,m(2))=1;
    end
end

% Compute frequencies and mode shapes
q=null(c); A=a*q; B=b*q; [modes,lam]=eig(B\A);
[lam,k]=sort(diag(lam)); w=sqrt(lam)'/dx;
modes=q*modes(:,k); modes=reshape(modes(:),nx,ny,neig);

% Plot first fifty approximate and exact frequencies
if nargin>4, return, end
m=1:min([50,length(w),length(wex)]);
pcter=100*(wex(m)-w(m))./wex(m);

clf; plot(m,wex(m),'k-',m,w(m),'k.',m,pcter,'k--')
xlabel('frequency number');
ylabel('frequency and % error')
legend('exact frequency','approx. frequency',...
    'percent error',2)
s=['MEMBRANE FREQUENCIES FOR AX / BY = ',...
    num2str(ax/by,5),' AND ',num2str(nx*ny),...
    ' GRID POINTS'];
title(s), grid on, shg
% print -deps recmemfr