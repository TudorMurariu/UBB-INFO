n = 100;
A = diag(2 * ones(n, 1)) + diag(-1 * ones(n - 1, 1), -1) + diag(-1 * ones(n - 1, 1), 1);
b = [1; zeros(n - 2, 1); 1];

[jac, it] = Jacobi(A, b)
[gauss, it] = Gauss_seidel(A, b)
omg = find_omega(A);
[my_sor, it] = SOR(A, b, omg)

real = A \ b;

norm(real-jac)/norm(real)
norm(real-gauss)/norm(real)
norm(real-my_sor)/norm(real)

# Concluzie: se poate observa faptul ca SOR rezolva cel mai rapid si mai corect sistemul, 
# urmat de Gauss_seidel si mai apoi de Jacobi