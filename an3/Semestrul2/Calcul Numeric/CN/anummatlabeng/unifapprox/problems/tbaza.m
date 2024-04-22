function A=tbaza(kn,t,n)
A=zeros(length(t),length(n));
for k=n
    A(:,k+1)=Bspline(3,kn,k,t);
end