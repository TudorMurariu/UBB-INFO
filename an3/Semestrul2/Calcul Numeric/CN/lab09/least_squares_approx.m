function res = least_squares_approx(x, y, functions, points)
    
    phi = functions(x);
    phi_approx = functions(points);
    
    n = length(x);
    [n, ~] = size(phi);
    
    % A = Z^T * Z ; B = Z^T * y ; unde Z^T e phi
    for i = 1 : n
        for j = 1 : n
            A(i, j) = phi(i, :) * transpose(phi(j, :));
        end
        B(i, 1) = phi(i, :) * transpose(y);
    end
    
    % A * a = B
    a = linsolve(A, B);
    
    res = transpose(a) * phi_approx;
endfunction