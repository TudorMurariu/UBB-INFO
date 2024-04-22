function y=reducereper(x)
%REDUCEREPER - reducere argument trigonometric la o perioada
syms doipi pi2 xs 
pi2=vpa(pi,1000);
doipi=vpa(2*pi2,1000);
xs=vpa(x,1000);
y=double(xs-floor(xs/doipi)*doipi);
