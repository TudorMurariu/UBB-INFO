function testNewtonEcuatie()
%test metoda lui Newton-Raphson pt ecuatii neliniare si pentru SISTEME

%Fie cos(x) = x => cos(x) - x = 0 => functia: f(x)=cos(x)-x
%f - functia  cos(x) - x
f = @(x) cos(x) - x;

%fd - derivata functiei
%in MATLAB, in consola: syms x;  f = cos(x)-x; diff(f)
fd = @(x) -sin(x) - 1;

%x0 - valoarea de pornire
x0 = pi/4;

%apel functie
%z - solutia aproximata a ecuatiei cos(x) - x = 0
%ni - nr de iteratii
[z,ni] = Newton(f,fd,x0,1e-4,0,50)


end
