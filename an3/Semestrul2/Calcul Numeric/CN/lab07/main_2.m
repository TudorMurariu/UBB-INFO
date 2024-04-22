nodes = [0 1 2];
nodevals = exp(nodes);

t = 1 : 0.01 : 2;
res = interpolareHermiteMultiplePoints(nodes, nodevals, nodevals, t);
res
t

plot(t, res, 'r');
hold on;
resexp = exp(t);
plot(t, resexp, 'b');
hold off;
pause