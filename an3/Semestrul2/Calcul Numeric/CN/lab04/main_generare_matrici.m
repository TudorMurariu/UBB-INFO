pkg load statistics

[A, B] = generare_matrici(10);

# Eliminare Gausiana
x1 = eliminare_gausiana(A, B)

# LUP
[L, U, P] = lup(A);
det(L);
U;
y = L \ (P * B);
x2 = U \ y 