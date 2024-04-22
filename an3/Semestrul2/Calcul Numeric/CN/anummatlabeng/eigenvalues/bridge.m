%BRIDGE
%input m n kappa
n=5; m=10; kappa=1;
s2=sqrt(2);
a=3+s2;
b=3/2+s2;
g=1+s2;
bv=b*ones(n-2,1);
M=m*diag([a;bv;a;g;bv;g]);
K12=-eye(n)-diag(ones(n-1,1),1)-diag(ones(n-1,1),-1);
d=[4;5*ones(n-2,1);4];
K11=diag(d)-diag(ones(n-1,1),1)-diag(ones(n-1,1),-1);
K=kappa*[K11, K12; K12, K11];
[Z,la]=eig(M,K);
la=diag(la)
w=sqrt(la)