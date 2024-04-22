n = 4;

[A, b] = generareSistemDiagDomiante(n)
err = 10^(-6);

sol_Jacobi = rezolvareJacobi(A, b, err)
sol_Gauss_Seidel = rezolvareGaussSeidel(A, b, err)
sol_SOR = rezolvareSOR(A, b, determinareOmega(A), err)