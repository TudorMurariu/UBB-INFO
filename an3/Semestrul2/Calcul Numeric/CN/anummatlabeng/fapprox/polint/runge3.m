function runge3(n,w)
%RUNGE3 - test for the convergence of Lagrange interpolation
%equispaced nodes 
%n- a vector of degrees, w- window
clf
xg=-5:0.1:5;
yg=1./(1+xg.^2);
plot(xg,yg,'k-','Linewidth',2);
hold on
nl=length(n);
ta=5*[-1:0.0005:-0.36,-0.35:0.01:0.35, 0.36:0.0005:1]';
ya=zeros(length(ta),nl);
leg=cell(1,nl+1); leg{1}='f';
for l=1:nl
    xn=5*[-1:2/n(l):1];
    yn=1./(1+xn.^2);
    ya(:,l)=lagr(xn,yn,ta);
    leg{l+1}=strcat('L_{',int2str(n(l)),'}');
end
plot(ta,ya)
axis(w)
legend(leg,-1)
