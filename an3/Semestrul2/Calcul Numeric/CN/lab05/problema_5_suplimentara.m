n = randi(25)

[A_dense, B_dense] = generare_matrici(n);
[A_spare, B_spare] = generare_matrici_rare(n, 0.5, 2, 50);

tic;
[jac, it] = Jacobi(A_dense, B_dense);
jacobi_dense = toc;
omg = find_omega(A_dense);
tic;
[my_sor, it] = SOR(A_dense, B_dense, omg);
sor_dense = toc;
tic;
[gauss, it] = Gauss_seidel(A_dense, B_dense);
gauss_dense = toc;

tic;
[jac, it] = Jacobi(A_spare, B_spare);
jacobi_spare = toc;
omg = find_omega(A_spare);
tic;
[my_sor, it] = SOR(A_spare, B_spare, omg);
sor_spare = toc;
tic;
[gauss, it] = Gauss_seidel(A_spare, B_spare);
gauss_spare = toc;

disp("Timpi pentru Jacobi:")
[jacobi_dense, jacobi_spare]
jacobi_dense-jacobi_spare<eps()
disp("Timpi pentru SOR:")
[sor_dense, sor_spare]
sor_dense-sor_spare<eps()
disp("Timpi pentru Gauss_Seidel:")
[gauss_dense, gauss_spare]
gauss_dense-gauss_spare<eps()

#In general, matricile dense sunt rezolvate mai rapid.
