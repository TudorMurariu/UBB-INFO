Aproximare integral? prin cuadratur? adaptiv?:
f - func?ia
a, b - limite de integrare
tol - toleran?a
met - cuadratura repetat? utilizat?
I - valoarea integralei
function I = adquad(f, a, b, tol, met)

    % Constanta convenabil? (4 sau 5):
    m = 4;
    
    I1 = met(f, a, b, tol, m);
    I2 = met(f, a, b, tol, 2 * m);
    
    if (abs(I1 - I2) < tol)
        I = I2;
    else 
        Ia = adquad(f, a, (a + b) / 2, tol, met);
        Ib = adquad(f, (a + b) / 2, b, tol, met);
        I = Ia + Ib;
    end

end
