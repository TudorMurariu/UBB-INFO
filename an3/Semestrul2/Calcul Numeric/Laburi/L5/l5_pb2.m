A = [6, -4, 1; 2, 6, 0; 2, -5, 8];
b = [1; 2; -5];
err = 10^(-6);
omega = determinareOmega(A)
rezolvareSOR(A, b, omega, err)