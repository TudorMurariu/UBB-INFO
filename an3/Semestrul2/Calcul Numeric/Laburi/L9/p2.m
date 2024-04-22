%p2
x = [-1.024940 -0.949898 -0.866114 -0.773392 -0.671372 -0.559524 -0.437067 -0.302909 -0.159493 -0.007464];
y = [-0.389269 -0.322894 -0.265256 -0.216557 -0.177152 -0.147582 -0.128618 -0.121353 -0.127348 -0.148895];

% Model parabolic
phi = @(x)[ones(1, length(x)); x.^2];

x_approx = x(1):(x(length(x)) - x(1)) / 100 : x(length(x));
y_approx = least_squares_approx(x, y, phi, x_approx);

plot(x, y, 'o', x_approx, y_approx, '-');
title("Traiectoria asteroidului", "FontSize", 14);