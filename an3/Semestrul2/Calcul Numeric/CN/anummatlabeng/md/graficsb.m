function graficsb(x,y,u,v,f)
%deseneaza graficul unui interpolant suma booleana
%x,y -nodurile
%u,v - punctele de pe grila
%f functia de interpolat
%apel graficsb(x,y,u,v,f)
mu=length(u);
nu=length(v);
for i=1:mu
   for j=1:nu
      z(i,j)=sumbool(u(i),v(j),x,y,f);
   end
end
surf(u,v,z)