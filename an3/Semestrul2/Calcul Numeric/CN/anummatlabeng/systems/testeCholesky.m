%teste Cholesky

%pascal(6)
A=pascal(6);
R=chol(A)        %rutina MATLAB
R1=Cholesky(A)   %rutina mea
R1'*R1-A         %verificare
%moler 5x5
B=gallery('Moler',5);
R2=Cholesky(B)   %rutina MATLAB
R=chol(B)        %rutina mea
R2'*R2-B         %verificare
%pascal 10x10
A=pascal(10)
R=Cholesky(A)
R3=chol(A)
R-R3
R'*R-A
