fprintf("Reprezentarea grafic? a f ?i Lmf (sin(x)):\n");
nodes = [0, 1];
nodevals = sin(nodes);

points = 0 : 0.01 : 1;
result = interpolare_Lagrange_multiple_points(nodes, nodevals, points);

plot(points, result, 'r');
hold on;

sin_result = sin(points);
plot(points, sin_result, 'b');
hold off;