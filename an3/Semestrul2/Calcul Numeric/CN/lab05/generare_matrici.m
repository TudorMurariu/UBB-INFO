function [A, b] = generare_matrici(n)
    max_element = 50;
    A = randi([-max_element, max_element],n);
    out = sum(abs(A),2) + randi(max_element, n,1);
    for i=1:n
        A(i,i) = out(i);
    end
    b = A\([1:n]');
end