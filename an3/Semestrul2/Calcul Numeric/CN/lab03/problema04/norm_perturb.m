function perturbations = norm_perturb(p)
% Generates normal perturbations to the coefficients of the polynomial
perturbations = randn(size(p)) * sqrt(1e-10);
end
