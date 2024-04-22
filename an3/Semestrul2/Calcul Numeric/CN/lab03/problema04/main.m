n = 20;
roots_p1 = 1:n;
p1 = poly(roots_p1); % polynomial associated with the first equation
p2 = 2 .^ -(1:n); % polynomial associated with the second equation

nc1 = condpol(p1, roots_p1) % condition number for the first equation
%plot the roots for p1
figure(1)
plot_p1 = plot(roots_p1, zeros(1, n), 'r.');
set(plot_p1, 'Markersize', 15); hold on;

% perturb coefficients
for i=1:20
  # norm distribution
  mean=0;
  sigma = 1e-10; 
  norm_perturbed_p1 = p1 + normrnd(mean,sigma,1, n + 1);
  norm_perturbed_roots_p1 = roots(norm_perturbed_p1);
  plot_norm_perturbed_p1 = plot(norm_perturbed_roots_p1, 'b.');
  hold on; set(plot_norm_perturbed_p1, 'Markersize', 10);

  #unif distribution
  unif_perturbed_p1 = p1 + unifrnd(mean, sigma, 1, n + 1);
  unif_perturbed_roots_p1 = roots(unif_perturbed_p1);
  plot_unif_perturbed_p1 = plot(unif_perturbed_roots_p1, 'g.');
  hold on; set(plot_unif_perturbed_p1, 'Markersize', 8);
endfor
  
nc2 = condpol(p2) % condition number for the second equation
roots_p2 = roots(p2);
%plot the roots for p1
figure(2)
plot_p2 = plot(roots_p2, zeros(1, length(roots_p2)), 'r.');
set(plot_p2, 'Markersize', 15); hold on

% perturb coefficients
for i=1:20
  % norm distribution
  mean=0;
  sigma = 1e-10; 
  norm_perturbed_p2 = p2 + normrnd(mean, sigma, 1, length(p2));
  norm_perturbed_roots_p2 = roots(norm_perturbed_p2);
  plot_norm_perturbed_p2 = plot(norm_perturbed_roots_p2, 'b.');
  set(plot_norm_perturbed_p2, 'Markersize', 12); hold on

  %unif distribution
  unif_perturbed_p2 = p2 + sigma .* rand(1, n);
  unif_perturbed_roots_p2 = roots(unif_perturbed_p2);
  plot_unif_perturbed_p2 = plot(unif_perturbed_roots_p2, 'g.');
  hold on; set(plot_unif_perturbed_p2, 'Markersize', 8);
endfor