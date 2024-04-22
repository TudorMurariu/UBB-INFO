function graficpt2(x,y,u,v,f)
%deseneaza graficul unui interpolant produs tensorial
%x,y -nodurile
%u,v - punctele de pe grila
%f functia de interpolat
%apel graficpt(x,y,u,v,f)
mu=length(u);
nu=length(v);
for i=1:mu
   for j=1:nu
      z(i,j)=prodtens(x,y,u(i),v(j),f);
   end
end
surf(u,v,z);