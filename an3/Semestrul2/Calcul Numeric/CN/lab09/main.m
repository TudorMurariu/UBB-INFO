x = 1 : 10;
y = sin(1 : 10);

phi = @(x)[ones(1, length(x)); x; x.^2; x.^3; x.^4; x.^5];

x_approx = x(1) : (x(length(x)) - x(1)) / 100 : x(length(x));
y_approx = least_squares_approx(x, y, phi, x_approx);

fprintf("Problema 1");
plot(x, y, 'o', x_approx, y_approx, '-');