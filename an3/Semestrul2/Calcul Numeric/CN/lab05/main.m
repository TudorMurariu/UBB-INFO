A = [ 3, 1, -1; 
      1, -4, 1; 
      2, 5, -4 ];
B = [ 10; 
      -2; 
      0 ];

[x,ni] = Jacobi(A, B)
omega=find_omega(A)
[x1, ni1] = SOR(A,B, omega)

n = 7;
A = diag(5 * ones(n, 1)) + diag(-1 * ones(n - 1, 1), -1) + diag(-1 * ones(n - 1, 1), 1);
b = 3 * ones(n, 1) + [1; zeros(n - 2, 1); 1];

[jac, it] = Jacobi(A, b)
omg = find_omega(A);
[my_sor, it] = SOR(A, b, omg)

ons = ones(n, 1);
A = diag(5 * ons) + diag(-1 * ons(2:end), -1) + diag(-1 * ons(2:end), 1) + diag(-1 * ons(4:end), -3) + diag(-1 * ons(4:end), 3);
b = ones(n, 1) + [2; 1; 1; zeros(n-6, 1); 1; 1;2]; 

[jac, it] = Jacobi(A, b)
omg = find_omega(A);
[my_sor, it] = SOR(A, b, omg)


[A, b] = generare_matrici(10000);

[jac, it] = Jacobi(A, b)
omg = find_omega(A);
[my_sor, it] = SOR(A, b, omg)
[gauss, it] = Gauss_seidel(A, b)