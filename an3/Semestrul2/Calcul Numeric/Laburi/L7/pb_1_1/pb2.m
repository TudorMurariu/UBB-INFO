addpath('../1_1');

x = [0.3, 0.32, 0.35];
f = sin(x);
fderiv = cos(x);
point = [0.34];

h = Hermite(x, f, fderiv, point);
fprintf("\nPRIMA APROXIMARE:");
fprintf("\nHermite: sin(%f) = %f", point, h);
fprintf("\nSoftware: sin(%f) = %f", point, sin(point));
fprintf("\nEROARE: %f", abs(sin(point) - h));

% Ad?ug?m punctul 0.33 la datele noastre:
x = [0.3, 0.32, 0.33, 0.35];
f = sin(x);
fderiv = cos(x);
point = [0.34];

h = Hermite(x, f, fderiv, point);
fprintf("\nA DOUA APROXIMARE:");
fprintf("\nHermite: sin(%f) = %f", point, h);
fprintf("\nSoftware: sin(%f) = %f", point, sin(point));
fprintf("\nEROARE: %f", abs(sin(point) - h));