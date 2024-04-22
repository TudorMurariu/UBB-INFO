function E=recinv(n,k)
%recurenta inversa

E=0;
for j=n+k:-1:n+1
    E=(1-E)/j;
end