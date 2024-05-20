f = @(t) exp(t);
a = 2; b = 3;
err = 1e-9;
nmax = 10;

expected = integral(f, a, b)
Romberg_computed = Romberg(f, a, b, err, nmax)