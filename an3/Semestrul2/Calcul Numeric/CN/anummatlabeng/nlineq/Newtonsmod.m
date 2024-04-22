function x=Newtonsmod(f,fd,x0,ea,er,Nmax)
%NEWTONS - metoda lui Newton pentru ecuatii in R
%modificare pentru a urmari executia
%Intrare
%f - functia
%fd - derivata
%x0 - valoarea de pornire
%ea,er - eroarea absoluta, respectiv relativa
%Nmax - numar maxim de iteratii
%Iesire
%z - aproximatia solutiei
%ni - numar de iteratii

if nargin<6, Nmax=50; end
if nargin<5, er=0; end
if nargin<4, ea=1e-3; end
x=[x0,zeros(1,Nmax)];
for k=1:Nmax
    x(k+1)=x(k)-f(x(k))/fd(x(k)); 
    if abs(x(k+1)-x(k))<ea+er*x(k+1) %succes
        return
    end
end
%esec
disp('s-a depasit numarul maxim de iteratii')
        
