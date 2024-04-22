%P3_2
n=input('n=');

%recurenta aplicata direct
E=recdir(n)

%recurenta inversa
k=19;
E=recinv(n,k)
%e cu precizia eps
e=1/recinv(1,k);
%verificare
(e-exp(1))/exp(1)
