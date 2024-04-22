function sailboat(alpha, beta, gamma, L)
%SAILBOAT - computation of wind action on
%           a sailboat mast
% Alfio Quarteroni, Riccardo Sacco, Fausto Saleri
% Numerical  Mathematics
% Springer 2000

f = @(x) alpha*x./(x+beta).*exp(-gamma*x);
xf = @(x) x.*f(x);
x=1:9;
err=10.^(-x);
for k=1:9
    [R1,ne1(k)]= adquad(f,0,L,err(k));
    [b1,neb1(k)]= adquad(xf,0,L,err(k)); b1=b1/R1;
    [R2,ne2(k)]= quad(f,0,L,err(k));
    [b2,neb2(k)]= quad(xf,0,L,err(k)); b2=b2/R2;
    [R3,ne3(k)]=quadl(f,0,L,err(k));
    [b3,neb3(k)]=quadl(xf,0,L,err(k)); b3=b3/R3;
end
subplot(1,2,1)
plot(x,ne1,'b-x',x,ne2,'r-+',x,ne3,'g--d')
xlabel('-log_{10}(err)','FontSize',14); ylabel('n','FontSize',14)
legend('adquad','quad','quadl',0)
title('Computation of \it{R}','FontSize',14)
subplot(1,2,2)
plot(x,neb1,'b-x',x,neb2,'r-+',x,neb3,'g--d')
xlabel('-log_{10}(err)','FontSize',14); ylabel('n','FontSize',14)
legend('adquad','quad','quadl',0)
title('Computation of \it{b}','FontSize',14)
R1,R2,R3
b1,b2,b3


