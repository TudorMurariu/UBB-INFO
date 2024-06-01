f = @(x) x - (1/2)*sin(x) - 10;
fd = @(x) 1 - (1/2)*cos(x); %Derivam, derivam
x0 = 0;
[a,b] = Newtons(f,fd,x0,1e-28,0,30); %Newtons din anummatlabeng
a
x1 = 1;
[c,d] = secant(f,x0,x1,1e-28,0,30);
c

%prob2 - toate combinatiile sistemului
x0 = [1;1;0];
x1 = [-1;1;0];
x2 = [1;-1;0];
x3 = [-1;-1;0];


f = @(x) [9*x(1)^2 + 36*x(2)^2 + 4*x(3)^2 - 36
          x(1)^2 - 2*x(2)^2 - 20*x(3)
          x(1)^2 - x(2)^2+ x(3)^2];%sistemul, pe verticala

fd = @(x) [18*x(1), 72*x(2), 8*x(3);
           2*x(1), -4*x(2), -20;
           2*x(1), -2*x(2), 2*x(3)];%derivatele la fiecare

Newton(f,fd,x0,1e-8,0,50)
Newton(f,fd,x1,1e-8,0,50)
Newton(f,fd,x2,1e-8,0,50)
Newton(f,fd,x3,1e-8,0,50)