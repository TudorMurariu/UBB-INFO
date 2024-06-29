%p3
% A - B - C - D
% AB = x1; BC = x2; CD = x3;
% X = [ AD; AC; BD; AB; CD]
X = [1 1 1; 1 1 0; 0 1 1; 1 0 0; 0 0 1];
Y = [89; 67; 53; 35; 20];

a = linsolve(X, Y);
AB = a(1,1);
BC = a(2,1);
CD = a(3, 1);
fprintf("AB = %7.2f, BC = %7.2f, CD = %7.2f\n", AB, BC, CD);
