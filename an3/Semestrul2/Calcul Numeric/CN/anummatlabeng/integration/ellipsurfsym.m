%ELIPSURFSYM 
clear
syms alpha beta K2 s2 x f vI
s2=sqrt(sym(2));
alpha=(s2-1)/10;
beta=sym(10);
K2=beta^2*sqrt(1-alpha^2*beta^2);
f=sqrt(1-K2*x^2);
vI=simplify(4*sym(pi)*alpha*int(f,0,1/beta))
vpa(vI,16)