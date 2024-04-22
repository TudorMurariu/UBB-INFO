Cuadratur? adaptiv? bazat? pe metoda lui Simpson ?i extrapolare.
Determin? valoarea aproximativ? a integralei:
f - func?ia
a, b - limitele de integrare
tol - toleran?a
I - valoarea aproximativ? a integralei
function Q = adquad2(f, a, b, tol)

    % Toleran?a va fi, default, 1E-3
    if (nargin < 4)
        tol = 1E-3; 
    end

    c = (a + b) / 2;
    fa = f(a);
    fb = f(b); 
    fc = f(c); 
    
    Q = quadstep(f, a, b, tol, fa, fc, fb);
   
end
