addpath('../1_1');
x = 0.25;
fprintf("Hermite: exp(%f) = %f", x, Hermite([0 1 2], [exp(0) exp(1) exp(2)], [exp(0) exp(1) exp(2)], x));
fprintf("Software: exp(%f) = %f", x, exp(x));