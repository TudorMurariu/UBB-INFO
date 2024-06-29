f = @(t) exp(t);
a = 2; b = 3;
err = 1e-9;

val_integrala = integral(f, a, b)
val_integrala_adquad = adquad(f, a, b, err)