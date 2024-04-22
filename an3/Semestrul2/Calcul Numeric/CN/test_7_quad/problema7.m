tolerances = [1e-1, 1e-2, 1e-3, 1e-4, 1e-5, 1e-6];
adquad_results = [];
romberg_results = [];

% Definirea functiei
F = @(x) 2./(1+x.^2);

for tol = tolerances
    % Quadratura adaptiva
    [Q_adquad, fcount_adquad] = adquad(F, -1, 1, tol);
    error_adquad = abs(Q_adquad - pi);
    adquad_results = [adquad_results; tol, error_adquad, fcount_adquad];
    
    % Metoda Romberg
    [Q_romberg, fcount_romberg] = Romberg(F, -1, 1, tol, 10);
    error_romberg = abs(Q_romberg - pi);
    romberg_results = [romberg_results; tol, error_romberg, fcount_romberg];
end

% Rezultatele plotate
% precizia masurata cu ajutorul erorii absolute
figure(1)
loglog(adquad_results(:,1), adquad_results(:,2), 'b*-');
hold on
loglog(romberg_results(:,1), romberg_results(:,2), 'r*-');
xlabel('Tolerance')
ylabel('Absolute Error')
legend('Adaptive Quadrature', 'Romberg Method')
title('Absolute Error vs Tolerance')

% numarul de evaluari de functie
figure(2)
loglog(adquad_results(:,1), adquad_results(:,3), 'b*-');
hold on
loglog(romberg_results(:,1), romberg_results(:,3), 'r*-');
xlabel('Tolerance')
ylabel('Function Evaluations')
legend('Adaptive Quadrature', 'Romberg Method')
title('Function Evaluations vs Tolerance')
