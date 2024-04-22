A = [
    1, 2, 1;
    2, 5, 3;
    1, 3, 3
    ];
B = [
    4;
    10;
    7
    ];

pkg load statistics
rezolvare_cholesky(A, B)

[A, B] = generare_matrici_herm(10);
rezolvare_cholesky(A, B)