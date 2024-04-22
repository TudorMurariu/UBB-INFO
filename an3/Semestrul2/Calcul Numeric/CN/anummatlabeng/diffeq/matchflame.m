delta=0.01;
F = @(t,y) y^2-y^3;
opts = odeset('RelTol',1e-4);
ode45(F,[0,2/delta],delta,opts);