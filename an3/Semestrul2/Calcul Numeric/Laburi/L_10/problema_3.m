% Adaptive quadrature
function I = problema_3(f, a, b, eps, g)

    % call I = adaptquad(f, a, b, eps, g)
    % f - integrand
    % a, b - endpoints
    % eps -tolerance
    % g - composed rule used on subintervals

    m = 4;
    I1 = g(f, a, b, m);
    I2 = g(f, a, b, 2 * m);

    if abs(I1 - I2) < eps % success
       I = I2;
       return
    else  % recursive sudivision
       I = problema_3(f, a, (a + b) / 2, eps, g) + problema_3(f, (a + b) / 2, b, eps, g);
    end
end
