function [qx,qy,qz]=Cox_deBoor_2D(n,m,u,v,dx,dy,dz,s,t)
%COX_DEBOOR_2D - algoritmul Cox - deBoor bidimensional
%n,m - gradul dupa x si dupa y, respectiv
%u,v - nodurile (0:2n+N si 0:2m+M)
%dx,dy,dz - coordonatele punctelor de control
%s,t - abscisele si ordonatele in care se face evaluarea

% determin pozitie puncte in raport cu nodurile
[lx,ly]=size(dx);
k = ones(size(s));
for ll = n+1:length(u)
    k(u(ll) <= s) = ll;
end
l = ones(size(t));
for ll = m+1:length(v)
    l(v(ll) <= t) = ll;
end

ls=length(s); lt=length(t);
[S,T]=meshgrid(s,t);
%[K,L]=meshgrid(k,l);

for ii=1:ls
    for jj=1:lt
        ux=S(ii,jj); vx=T(ii,jj);
        %kc=K(ii,jj); lc=L(ii,jj);
        %aplic Cox-deBoor dupa ordonata
        pcv=zeros(3,lx);
        for i=1:lx
            pcv=[dx(:,i)';dy(:,i)';,dz(:,i)'];
            pcu(:,i)=Cox_deBoor(m,pcv,v,vx);
        end
        %aplic Cox-deBoor dupa abscisa
        qq=Cox_deBoor(n,pcu,u,ux);
        qx(ii,jj)=qq(1); qy(ii,jj)=qq(2); qz(ii,jj)=qq(3);
    end
end
