function HH=HessenRQ(H)
%HESSENRQ - computes RQ transform of a Hessenberg matrix
%using Givens rotations
%input H - Hesenberg matrix
%output HH - RQ transform of H

[m,n]=size(H);
Q=eye(m,n);

for k=2:n
    a=H(k-1:k,k-1);
    an=sqrt(a'*a);               %Euclidean norm
    c=sign(a(2))*abs(a(1))/an;   %sine
    s=sign(a(1))*abs(a(2))/an;   %cosine
    Jm=eye(n);
    Jm(k-1,k-1)=c; Jm(k,k)=c;
    Jm(k-1,k)=s; Jm(k,k-1)=-s;
    H=Jm*H;
    Q=Q*Jm';
end
HH=H*Q;
    