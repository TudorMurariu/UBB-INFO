
# Func?ie de aproximare a valorii x, folosind interpolarea Hermite.

function [result] = Hermite(x, f, fderiv, point)

    % Ini?ializare
    [~, m] = size(x);
    Q = zeros(2 * m + 2, 2 * m + 2);
    z = zeros(2 * m + 2);
    
    for i = 1 : m
        % Pasul P2:
        z(2 * i - 1) = x(i);
        z(2 * i) = x(i);
        Q(2 * i - 1, 1) = f(i);
        Q(2 * i, 1) = f(i);
        Q(2 * i, 2) = fderiv(i);

        % Pasul P3:
        if (i ~= 1)
            Q(2 * i - 1, 2) = (Q(2 * i - 1, 1) - Q(2 * i - 2, 1)) / (z(2 * i - 1) - z(2 * i - 2));
        end
    end
    
    for i = 3 : 2 * m
       for j = 3 : i
          Q(i, j) = (Q(i, j - 1) - Q(i - 1, j - 1)) / (z(i) - z(i - j + 1)); 
       end
    end
    
    s = 1;
    p = Q(1, 1);
    for i = 2 : 2 * m
       a = abs(x(floor(i / 2)) - point);
       s = s * a;
       prev = p;
       p = p + s * Q(i, i);
       result = p;
       if (prev - p < 1E-5)
          break 
       end
    end

end
