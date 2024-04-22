point = [0 pi/4 pi/2 3*pi/2]
nodevals = sin(point)

t = -pi : 0.01 : pi
hermite_results = Hermite_multiplePoints(point, nodevals, nodevals, t);

fprintf("Reprezentarea grafic a sin(x) ?i a polinomului s?u de interpolare Hermite:");
hold on;
results = sin(t);
plot(t, hermite_results, 'r');
plot(t, results, 'b');
hold off;