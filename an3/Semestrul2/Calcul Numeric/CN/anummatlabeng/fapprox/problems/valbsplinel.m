function y=valbsplinel(c,t,x)
%VALBSPLINEL - valoare aproximanta B-spline liniar
%apel y=valbsplinel(c,x)
%c - coeficienti
%t - punctele
%x - nodurile
y=zeros(size(t));
for k=1:length(c)
    y=y+c(k)*bsplinel(t,k,x);
end