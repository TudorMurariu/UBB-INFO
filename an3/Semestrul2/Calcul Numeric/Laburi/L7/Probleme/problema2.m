nodes = [0.8 1 1.4 1.8 2.1];
nodevals = exp(nodes);

t = 1 : 0.01 : 7;

res = hermite(nodes, nodevals, nodevals, t);
plot(t, res, 'Color', 'red');
hold on;

f = exp(t);
plot(t, f, 'Color', 'blue');
hold off;
