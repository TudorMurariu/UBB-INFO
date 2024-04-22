function perturbations = unif_perturb(p)
% Generates uniform perturbations to the coefficients of the polynomial
perturbations = rand(size(p)) * 2e-5 - 1e-5;
end
