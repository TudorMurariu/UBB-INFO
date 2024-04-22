%P1_7
L=input('introduceti L (>=1)');
%metoda 1
S=0; n=0;
while S<L
    n=n+1;
    S=S+n^2;
end
n1=n-1
%metoda 2
r=roots([2,3,1,-6*L]);
n2=floor(max(r(imag(r)==0))-eps)
