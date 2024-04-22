# Subpunctul (a)
a = 0;
b = 2 * pi;

f = @(x) exp(sin(x) + 1 / sqrt(2) * cos(x));
df = @(x) (cos(x) - 1 / sqrt(2) * sin(x)) * f(x); 

m = 11;
k = 1:m;
x_Chebisev = (a+b)/2 + ((b-a)/2) * cos(pi * (2 * k - 1) / (2 * m));


dy_Chebisev = arrayfun(df, x_Chebisev);
y_Chebisev = arrayfun(f, x_Chebisev);

x_plot = linspace(a, b, 1000);
y_plot = f(x_plot);

Lagrange_plot = Lagrange(x_Chebisev, y_Chebisev, x_plot);
Hermite_plot = interpolareHermiteMultiplePoints(x_Chebisev, y_Chebisev, dy_Chebisev, x_plot);

figure;
hold on;
plot(x_plot, y_plot, 'g--', 'LineWidth', 6);
plot(x_plot, Lagrange_plot, 'r-.', 'LineWidth', 3);
plot(x_plot, Hermite_plot, 'b', 'LineWidth', 1);
plot(x_Chebisev, y_Chebisev, 'o', 'MarkerSize', 6, 'LineWidth', 1);
legend('Functia f', 'Interpolare Lagrange', 'Interpolare Hermite', 'Nodurile Cebisev');
xlabel('x');
ylabel('y');
title('Functia f si interpolarile Lagrange si Hermite');
hold off;

# Subpunctul (b)
t = pi/7;
f_t = f(t);
Lagrange_t = Lagrange(x_Chebisev, y_Chebisev, t);
Hermite_t = interpolareHermite(x_Chebisev, y_Chebisev, dy_Chebisev, t);
fprintf("Valoarea lui f(t): %.10f\n", f_t);
fprintf("Aproximarea f(t) folosind interpolarea Lagrange: %.10f\n", Lagrange_t);
fprintf("Aproximarea f(t) folosind interpolarea Hermite: %.10f\n", Hermite_t);


# Subpunctul (c)
# calculam eroarea, practic
Lagrange_error = norm(f_t - Lagrange_t);
Hermite_error = norm(f_t - Hermite_t);
fprintf("Eroarea interpolarii Lagrange: %.10f\n", Lagrange_error);
fprintf("Eroarea interpolarii Hermite: %.10f\n", Hermite_error);


# calculam eroarea, teoretic
points = linspace(a,b,1000);
y_points=f(points);
xi=max(abs(y_points));

syms x;
f_sym = exp(sin(x) + cos(x) * 1 / sqrt(2));
f_d = diff(f_sym, x, m + 1);  % Derivata de ordin m + 1 a functiei
f_d_e = double(subs(f_d, x, xi));

R = 1;
for i = 1:m 
  R *= abs(t - x_Chebisev(i));  % Calculam valoarea absoluta
end

R /= factorial(m+1);
R *= f_d_e;
fprintf("Eroarea interpolarii Lagrange calculata teoretic: %.10f\n", R);

n = m * 2;
x_rep = repelem(x_Chebisev, 2);
f_d = diff(f_sym, x, n + 1);  % Derivata de ordin n + 1 a functiei
f_d_e = double(subs(f_d, x, xi));

R = 1;
for i = 1:n
  R *= abs(t - x_rep(i));  % Calculam valoarea absoluta
end

R /= factorial(n + 1);
R *= f_d_e;
fprintf("Eroarea interpolarii Hermite calculata teoretic: %.10f\n", R);