fprintf("Graphic Representation:");
nodes = [1 1.3 1.6 1.8];
m = 4;

labels = {};
for k = 1 : m
  t = 1 : 0.01 : 2;
  p = evaluate_polynomial_multiple_points(nodes, k, t);
  labels = [labels num2str(k)];
  plot(t, p, 'color', rand(1,3))
  hold on;
endfor
legend(labels);
hold off;