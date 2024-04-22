function x=EGtrid(a,b,c,d)
%EGTRID - eliminare gaussiana fara pivot, sistem tridiagonal
%apel x=EGtrid(a,b,c,d)
%a - subdiagonala
%b - diagonala
%c - supradiagonala
%d - termenii liberi

n=length(d); x=zeros(n,1);
%eliminare
for k=1:n-1
    m=a(k)/b(k); %multiplicatorul
    b(k+1)=b(k+1)-m*c(k);
    d(k+1)=d(k+1)-m*d(k);
end
%substitutie inversa
x(n)=d(n)/b(n);
for k=n-1:-1:1
    x(k)=(d(k)-c(k)*x(k+1))/b(k);
end