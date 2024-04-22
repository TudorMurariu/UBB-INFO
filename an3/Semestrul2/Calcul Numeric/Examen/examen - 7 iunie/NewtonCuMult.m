function [z, k] = NewtonCuMult(f, fd, x0, m, iter_max)
    if nargin < 5; iter_max = 500; end
    er = 0;
    ea = 1e-3;
    
    x_prev = x0;
    for k = 1 : iter_max
        x_curr = x_prev - m * fd(x_prev) \ f(x_prev)
        if norm(x_curr - x_prev, inf) < ea + er * norm(x_curr, inf) % ok
            z = x_curr;
            return
        end
        x_prev = x_curr;
    end

    error('numarul maxim de iteratii depasit') % eroare
end