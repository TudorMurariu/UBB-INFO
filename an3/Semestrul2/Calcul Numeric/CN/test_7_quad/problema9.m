% Subpunctul (a) si (b)

syms x f(x) a b
R = RombergSym(f, a, b, 3);
disp("Formula lui Simpson:")
S = prod(factor(simplify(R(2, 2))))
disp("Formula Boole-Villarceau:")
BV = prod(factor(simplify(R(3, 3))))

% Subpunctul (c)
% calculam simbolic integrala de la 1 la 2 din ln(x) dx
I = int(log(x), 1, 2);
int_ln = double(I);
n = 9; a = 1; b = 2;
RMat = RombergMat(@log, 1, 2, n, 3);
err_Simpson = abs(RMat(2: end, 2) - int_ln);
err_BV      = abs(RMat(3: end, 3) - int_ln);
h = (b - a) ./ 2 .^ (0: (n - 1));
loglog(h(2: end), err_Simpson, "-o;Eroare Simpson;", h(3: end), err_BV, "-s;Eroare Boole-Villarceau;")
