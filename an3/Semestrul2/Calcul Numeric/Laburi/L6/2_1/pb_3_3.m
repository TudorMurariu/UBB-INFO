addpath('../1_4');

fprintf("Aproximarea sqrt(115):");
x = 115;
fprintf("f(%f) = %f", x, interpolare_Lagrange_known_function(x, @sqrt, 4, [100 107 117 121]));
fprintf("sqrt(%f) = %f", x, sqrt(x));