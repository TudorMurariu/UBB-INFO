% define A and b
A = randi(10, 10, 2);
b = sum(A, 2);

%rezolve using Cholesky
ATA = A' * A;
AB = A' * b;
R = cholesky(ATA);
y = (R.') \ AB;
x = R\y; 

disp("Solutia obtinuta folosind Cholesky:") 
x
disp("Solutia obtinuta folosind backslash:") 
backslash = A \ b
disp("Eroarea absoluta este:")
norm(backslash - x, inf)

% compute the condition number of A
disp("Conditionarea lui A:")
condA = cond(A)

% compute the condition number of A^T * A
ATA = A' * A;
disp("Conditionarea lui ATA:")
condATA = cond(ATA)

% compare the condition numbers
if condATA > condA
    disp('A e mai bine conditionata.');
else
    disp('ATA e mai bine contitionata.');
end

format long
norm(condATA - condA ^ 2, inf)
% cond AT * A = cond A ^ 2 deoarece eroarea se apropie foarte tare de 0.