Determinarea aproximativ? a integralei prin metoda Romberg:
f - func?ia
a, b - limite de integrare
prec - precizia calculului
nmax - num?rul de intera?ii
I - valoarea integralei
function I = Romberg(f, a, b, prec, nmax)

    % Num?rul default de itera?ii este 10
    if (nargin < 5)
        nmax = 10;
    end
    
    % Precizia default este 1E-3
    if (nargin < 4)
        prec = 1E-3;
    end
    
    h = b - a;
    
    R(1, 1) = h / 2 * (f(a) + f(b));
    I = R;
    
    for k = 2 : nmax    
        x = a + ([1 : 2^(k - 2)] - 1 / 2) * h;
        R(k, 1) = 1 / 2 * (R(k - 1, 1) + h * sum(f(x)));
        pwr = 4;
        for j = 2 : k
            R(k, j) = (pwr * R(k,j - 1) - R(k - 1, j - 1)) / (pwr - 1);
            pwr = pwr * 4;
        end
        if (abs(R(k, k) - R(k - 1, k - 1)) < prec)
            I = R(k, k);
            return
        end
        
        h = h / 2;
    end

end
