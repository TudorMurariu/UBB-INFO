%P9_6
close all
N=21;
d=ones(N-1,1);
T=2*eye(N)-diag(d,1)-diag(d,-1);
[V,D]=eig(T);
e=diag(D);
plot(e,'-o')
title('Eigenvalues of T_{21}')
figure(2)
nd=[1,5,2,11,3,21];
for k=1:length(nd)
    subplot(3,2,k)
    plot(V(:,nd(k)),'-o')
    title(['eigenvector ',num2str(nd(k))])
end
