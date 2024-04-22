f_sin = @(t) sin(t.^2);
f_cos = @(t) cos(t.^2);

n = 10;
a = 0;
b = pi;

% Aplicarea functiei Gauss_Legendre_ab pentru a aproxima integralele
integral_sin_10 = Gauss_Legendre_ab(f_sin, n, a, b);
integral_cos_10 = Gauss_Legendre_ab(f_cos, n, a, b);

disp('Integrala functiei sin(t^2) este aproximativ: ');
disp(integral_sin_10);
disp('Integrala functiei cos(t^2) este aproximativ: ');
disp(integral_cos_10);
disp('Eroare sin: ')
disp(abs(integral(f_sin, 0, pi) - integral_sin_10));
disp('Eroare cos: ')
disp(abs(integral(f_cos, 0, pi) - integral_cos_10));
