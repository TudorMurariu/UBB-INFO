%P11_1
g1=@(y) sin(2*pi*y);
g2=@(y) -y.*(y-1);
g3=@(x) -x.*(x-1);
g4=@(x) sin(2*pi*x);
N=20; M=20;
[X,Y,Z]=Coons(g1,g2,g3,g4,0,1,N,0,1,M);
mesh(X,Y,Z)
xlabel('x'), ylabel('y')
x=linspace(0,1,N); y=linspace(0,1,M);
zg1=g1(y); zg2=g2(y);
zg3=g3(x); zg4=g4(y);
hold on
plot3(zeros(size(x)),y,zg1,'b-','LineWidth',2)
plot3(ones(size(x)),y,zg2,'b-','LineWidth',2)
plot3(x,zeros(size(y)),zg3,'b-','LineWidth',2)
plot3(x,ones(size(y)),zg4,'b-','LineWidth',2)