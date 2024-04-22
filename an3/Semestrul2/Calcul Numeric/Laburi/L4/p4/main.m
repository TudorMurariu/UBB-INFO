A = [1, 2, 1;
     2, 5, 3;
     1, 3, 3];
b = [4; 10; 7];
disp(rezolvareCholesky(A, b))

disp("   ---    ")

[A,b] = generareSistem(3);
disp(rezolvareCholesky(A, b))
