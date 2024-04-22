m = 11;
t = pi / 7;

a = 0;
b = 2 * pi;

f = @(x) exp(sin(x) + 1 / sqrt(2) * cos(x));
fd = @(x) exp(sin(x) + (2 .^ (1 / 2) .* cos(x)) ./ 2) .* (cos(x) - (2 .^ (1 / 2) .* sin(x)) ./ 2);

format long;
sym_x = sym('sym_x');

sym_f = f(sym_x);

sym_fd11 = diff(sym_f, sym_x, m + 1);
fd11 = function_handle(-sym_fd11);
[max_xfd11, max_yfd11] = fminbnd(fd11, a, b);

sym_fd22 = diff(sym_f, sym_x, 2 * m + 1);
fd22 = function_handle(-sym_fd22);
[max_xfd22, max_yfd22] = fminbnd(fd22, a, b);

xx = (a:0.01:b)';
f_exact = f(xx);

x = sort(cos((0 : m)' * pi / m)) * (b - a) / 2 + (a + b) / 2;

c = [1/2; ones(m - 1, 1); 1/2] .* (-1) .^ ((0: m)');

y = f(x);

disp("\n Lagrange: ");
ffl = barycentricInterpolation(x, y, xx, c);
Lagrange_ft = barycentricInterpolation(x, y, [t], c)
errLagrange = abs(f(t) - Lagrange_ft)
errLagrangeTeor = abs(prod(t - x) * max_yfd11 / factorial(m + 1))

disp("\n Hermite noduri duble:");
yd = fd(x);
ff2 = hermiteNum(xx, x, y, yd);
Hermite_ft = hermiteNum([t], x, y, yd)
errHermite = abs(f(t) - Hermite_ft)
errHermiteTeor = abs(prod((t - x) .^ 2) * max_yfd22 / factorial(2 * m + 1))

clf; hold on; grid on;
plot(x, y, 'x', 'DisplayName', 'Data points');
plot(xx, f_exact, 'r-', 'LineWidth', 2, 'DisplayName', 'f');
plot(xx, ffl, '-', 'DisplayName', 'Lagrange');
plot(xx, ff2, '-', 'DisplayName', 'Hermite');
legend; % This command displays the legend