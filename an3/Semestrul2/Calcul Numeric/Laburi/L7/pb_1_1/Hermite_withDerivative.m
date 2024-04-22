
function [H, dH] = Hermite_withDerivative(x, f, fderiv, X)
	
    coefs = dif_div_duble(x, f, fderiv);
    coefs = coefs(1, :);
    
	z = repelem(x, 2);
	H = X; 
	dH = X;
    for k = 1 : length(X)
        P = 1; DP = 0; 
        H(k) = coefs(1) * P;
        dH(k) = 0;

        for i = 2 : length(coefs)
            DP = DP * (X(k) - z(i - 1)) + P;
            P = P * (X(k) - z(i - 1));
            H(k) = H(k) + coefs(i) * P;
            dH(k) = dH(k) + coefs(i) * DP;
        end
    
    end

end
