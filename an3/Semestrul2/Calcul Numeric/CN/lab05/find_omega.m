%function omega=find_omega(A) 
%  D = diag(diag(A));
%  L = tril(A, -1);
%  U = triu(A, 1);
%  T = inv(D)*(L + U);
%  rho_T = max(abs(eig(T)));
%  omega = 2 / (1 + sqrt(1 - rho_T^2));
%endfunction

function omega=find_omega(A)
    M = diag(diag(A)); 
    N = M-A;
    T = M\N;
    e = eig(T);
    rt = max(abs(e)); % raza spectrala a matricei Jacobi
    omega = 2/(1+sqrt(1-rt^2));
endfunction