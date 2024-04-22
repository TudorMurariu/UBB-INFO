f = @(x) x.^2 .* sin(x);
fd = @(x) 2 .* x .* sin(x) + x.^2 .* cos(x);
fdd = @(x) 2 .* sin(x) + 4 .* x .* cos(x) - x.^2 .* sin(x);  

fd1=fd([-2*pi,2*pi]);
fd2=fdd([-2*pi,2*pi]);

% Subpunctul (a)
n=12;
t=linspace(-2*pi,2*pi,1000);
ft=f(t');

% noduri echidistante
x_echidistant=linspace(-2*pi, 2*pi, n+1);
y_echidistant=f(x_echidistant);

cc1=CubicSplinec(x_echidistant,y_echidistant,0,fd1);
cc2=CubicSplinec(x_echidistant,y_echidistant,1,fd2);
cc3=CubicSplinec(x_echidistant,y_echidistant,2);
cc4=CubicSplinec(x_echidistant,y_echidistant,3);
z1=evalsplinec(x_echidistant,cc1,t);
z2=evalsplinec(x_echidistant,cc2,t);
z3=evalsplinec(x_echidistant,cc3,t);
z4=evalsplinec(x_echidistant,cc4,t);

figure(1)
subplot(2,1,1)
plot(x_echidistant,y_echidistant,'o',t,f(t),t,[z1,z2,z3,z4])
legend('noduri','f','complete','D2','natural','deBoor','Location','bestoutside');
title('spline')
xlim([-7, 7])
subplot(2,1,2)
plot(t',abs(repmat(ft,1,4)-[z1,z2,z3,z4]))
legend('complet','D2','natural','deBoor','Location','bestoutside');
title('error')
xlim([-7, 7])

% noduri Cerbisev de speta a II-a
a = -2*pi; 
b = 2*pi;
k = 0:n-1;
x_chebyshev = a + (b - a)/2 * (1 - cos((2*k+1)*pi/(2*n)));
y_chebyshev = f(x_chebyshev);

fd1=fd([-2*pi,2*pi]);
fd2=fdd([-2*pi,2*pi]);
cc1=CubicSplinec(x_chebyshev,y_chebyshev,0,fd1);
cc2=CubicSplinec(x_chebyshev,y_chebyshev,1,fd2);
cc3=CubicSplinec(x_chebyshev,y_chebyshev,2);
cc4=CubicSplinec(x_chebyshev,y_chebyshev,3);
z1=evalsplinec(x_chebyshev,cc1,t);
z2=evalsplinec(x_chebyshev,cc2,t);
z3=evalsplinec(x_chebyshev,cc3,t);
z4=evalsplinec(x_chebyshev,cc4,t);

figure(2)
subplot(2,1,1)
plot(x_chebyshev,y_chebyshev,'o',t,f(t),t,[z1,z2,z3,z4])
legend('noduri','f','complete','D2','natural','deBoor','Location','bestoutside');
title('spline')
xlim([-7, 7])
subplot(2,1,2)
plot(t',abs(repmat(ft,1,4)-[z1,z2,z3,z4]))
legend('complet','D2','natural','deBoor','Location','bestoutside');
title('error')
xlim([-7, 7])


# Subpunctul (b)
n = 11;
a = -2*pi;
b = 2*pi;
x = linspace(a, b, n);
y = f(x);
x_approx = linspace(min(x), max(x), 100);
functions = @(x) [ones(size(x)); x; x.^2; x.^3; x.^4; x.^5; x.^6; x.^7;];
res = least_squares_approx(x, y, functions, x_approx);
y_approx = f(x_approx);


figure(3)
hold on;
plot(x, y, 'o', t, f(t), x_approx, res);
legend('noduri','f', 'aproximanta least squares');
title('least squares')
xlim([-7, 7])