%P9_7
A=[6, 5, -5; 2, 6, -2; 2, 5, -1];
x0=[-1; 1; 1];
[xv,la]=Power_method(A,x0)
A=[2, 0, -1; -2, -10, 0; -1, -1, 4]; x0=ones(3,1);
[xv,la]=Power_method(A,x0);
disp('spectral radius') 
disp(abs(la))
