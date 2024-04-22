function q=Cox_deBoor(grad,pc,knots,t)
%COX_DEBOOR algoritmul Cox-DeBoor pentru B-spline
%apel q=Cox_deBoor(grad,pc,knots,t)
%grad- gradul spline-ului
%pc - poligonul de control
%knots - nodurile
%t - abscisele punctelor in care calculam spline-ul

n=length(knots);
knots=knots(:); t=t(:);
lt=length(t);
[lin,n]=size(pc);
% determin pozitie puncte in raport cu nodurile
j = ones(size(t));
for ll = grad+1:n
    j(knots(ll) <= t) = ll;
end
%aplicare efectiva algoritm
for l=1:lt
    qn=pc;
    for r=0:grad-1
        i=j(l)-grad+r+1:j(l);
        w=repmat(omegav(i,grad-r,knots,t(l)),lin,1);
        qn(:,i)=w.*qn(:,i)+(1-w).*qn(:,i-1);
    end
    q(:,l)=qn(:,j(l));
end
%------------
function u=omegav(i,k,knots,t)
u=zeros(size(i));
v1=find(knots(i+k) ~= knots(i));
u(v1)=(t-knots(i))./(knots(i+k)-knots(i));
