function omega=reloptgen(A)
%determina valoarea optima a parametrului
%relaxarii
M=diag(diag(A)); %determin matricea Jacobi
N=M-A;
T=M \ N;
if issparse(A)
    e=eigs(T,'lm');
else
    e=eig(T);
end
rt=max(abs(e)); %raza spectrala a matricei Jacobi
omega=2/(1+sqrt(1-rt^2));
