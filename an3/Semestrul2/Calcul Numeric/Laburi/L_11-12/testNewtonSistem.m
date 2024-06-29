function testNewtonSistem()
%test metoda lui Newton-Raphson pentru SISTEME
%fie sistemul: 
%       x1^2 + x2^2 - 1 = 0
%       x1^3 - x2 = 0
%Jacobianul - se deriveaza fiecare ecuatie in functie de toate
%necunoscutele.

%f - functia combinata ale celor 2 ecuatii ale sistemului
f = @(x) [x(1)^2 + x(2)^2 - 1; x(1)^3 - x(2)];

%fd - derivata functiei (adica Jacobianul)
fd = @(x) [2*x(1), 2*x(2); 3*x(1)^2, -1];

%x0 - valoarea de pornire, atat pentru x(1), cat si pt x(2)
x0 = [1;1];

%apel functie
%z - solutia aproximata a ecuatiilor
%ni - nr de iteratii
[z,ni] = Newton(f,fd,x0,1e-8,0,50)


end
