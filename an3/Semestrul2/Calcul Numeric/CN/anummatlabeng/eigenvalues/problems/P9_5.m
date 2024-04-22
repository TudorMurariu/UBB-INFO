%P9_5
close all
p=@(x) -2./x;
q=@(x) 2./x.^2;
r=@(x) sin(log(x))./x.^2;
a=1; b=2;
N=input('N=');
A=bilocalmat(p,q,r,a,b,N);
A=full(A);
[V,D]=eig(A);
e=diag(D);
plot(e,'-o')
figure(2)
nd=[1,2, fix(N/2), fix(N/2)+1,  N-1,N];
for k=1:length(nd)
    subplot(3,2,k)
    plot(V(:,nd(k)),'-o')
    title(['Eigenvector ',num2str(nd(k))])
end