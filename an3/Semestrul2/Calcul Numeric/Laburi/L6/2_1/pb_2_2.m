addpath('../1_5');

f = @(x)exp(x.^2 - 1);
x = 1.25;
fprintf("f(%f) = %f \n", x, interpolare_Lagrange_known_function(x, f, 4, [1 1.1 1.2 1.3 1.4]));
fprintf("!!f(%f) = %f \n", x, exp(x.^2 - 1));