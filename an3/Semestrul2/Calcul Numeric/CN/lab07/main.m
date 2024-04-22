pkg load symbolic;

syms x a b c z;
t = [a, b, c];
r = [1, 0, 1];

simplify(HermitePoly(t, r, z, sin(z)))