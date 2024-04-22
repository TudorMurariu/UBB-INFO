function A=genmieg(n)
A=tril(-ones(n,n),-1)+eye(n);
A(:,end)=ones(n,1);