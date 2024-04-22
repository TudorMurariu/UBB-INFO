% Fie f: [-2, 2] -> R, f(x) = x^2
addpath('../1_1');
x = [-2 2];
f = [4 4]; 
fderiv = [-4 4];

t = -2 : 0.01 : 2;
results = Hermite_multiplePoints(x, f, fderiv, t);

fprintf("Reprezentarea grafic? a unei cubice parametrice Hermite (x^2):")
plot(t, results, 'r');