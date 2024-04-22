%ELLIPSURF - surface of an ellipsoid

err=1e-8;
beta=10;
alpha=(sqrt(2)-1)/10;
alpha2=alpha^2;
beta2=beta^2;
K2=beta2*sqrt(1-alpha2*beta2);
f=@(x) sqrt(1-K2*x.^2);
fpa=4*pi*alpha;
[vi(1),nfe(1)]=Romberg(f,0,1/beta,err,100);
[vi(2),nfe(2)]=adquad(f,0,1/beta,err);
[vi(3),nfe(3)]=quad(f,0,1/beta,err);
[vi(4),nfe(4)]=quadl(f,0,1/beta,err);
vi=fpa*vi;
meth={'Romberg','adquad','quad','quadl'};
for i=1:4
    fprintf('%8s %18.16f %3d\n',meth{i},vi(i),nfe(i))
end

