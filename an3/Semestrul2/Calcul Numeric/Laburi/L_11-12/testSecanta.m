function testSecanta()
%test secanta

%f - functia cos(x) - x
f = @(x) cos(x) - x;
%x0,x1 - valorile de pornire
x0 = 0.5;
x1 = pi/4;

%apelul functiei
%z - radacina aproximativa a ec. cos(x)-x = 0
%ni - nr de iteratii
[z,ni] = secant(f,x0,x1,1e-5,0,50)


end
