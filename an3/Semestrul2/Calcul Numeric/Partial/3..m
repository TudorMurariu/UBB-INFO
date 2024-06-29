% Approximates the integral of sqrt(3t-t^2-2) * sin(t) from 1 to 2
% using Gauss-Chebyshev Type 2 quadrature.

% Define the integrand as an anonymous function
original_f = @(t) sqrt(3.*t - t.^2 - 2) .* sin(t);

% Change of variables to transform the integral into the [-1, 1] interval.
% Linear transformation from [1, 2] to [-1, 1]: t = (3/2) * (x + 1) + 1
transformed_f = @(x) original_f((3/2) * (x + 1) + 1) * (3/2);

% Weights and abscissas for Gauss-Chebyshev Type 2 quadrature (n=2)
% For n=2, the abscissas are the roots of the Chebyshev polynomial of the second kind, U_2(x)
abscissas = [-sqrt(3)/2, sqrt(3)/2];
weights = [pi/2, pi/2];

% Perform the Gauss-Chebyshev quadrature
n = length(abscissas); % Number of abscissas and weights
integral_approximation = 0;
for i = 1:n
    integral_approximation += weights(i) * transformed_f(abscissas(i));
end

% Display the result
disp(['The approximated integral using Gauss-Chebyshev Type 2 quadrature is: ', num2str(integral_approximation)]);

