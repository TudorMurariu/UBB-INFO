function x=EGpivtrid(a,b,c,d)
%EGPIVTRID - eliminare gaussiana cu pivot, sistem tridiagonal
%apel x=EGpivtrid(a,b,c,d)
%a - subdiagonala
%b - diagonala
%c - supradiagonala
%d - termenii liberi

n=length(d); x=zeros(n,1); cn=zeros(n-1,1);
%pivotarea mai poate adauga o diagonala
%eliminare
for k=1:n-1
    %pivotare
    if abs(b(k))<abs(a(k)) % interschimbare
        [b(k),a(k)]=swap(b(k),a(k));
        [c(k),b(k+1)]=swap(c(k),b(k+1));
        if k<n-1
            [cn(k),c(k+1)]=swap(cn(k),c(k+1));
        end
        [d(k),d(k+1)]=swap(d(k),d(k+1));
    end
    m=a(k)/b(k); %multiplicatorul
    b(k+1)=b(k+1)-m*c(k);
    if k<n-1
        c(k+1)=c(k+1)-m*cn(k);
    end
    d(k+1)=d(k+1)-m*d(k);
end
%substitutie inversa
x(n)=d(n)/b(n);
x(n-1)=(d(n-1)-c(n-1)*x(n))/b(n-1);
for k=n-2:-1:1
    x(k)=(d(k)-c(k)*x(k+1)-cn(k)*x(k+2))/b(k);
end
function [xs,ys]=swap(x,y)
t=x; xs=y; ys=t;