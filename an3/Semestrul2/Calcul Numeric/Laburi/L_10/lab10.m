

P4. Implementa?i metoda lui Romberg.
% Aproxim?m integrala func?iei sin(x) pentru intervalul [0, pi]:
f = @(x)sin(x);
a = 0;
b = pi;
fprintf("Romberg:");
fprintf("Valoarea integralei date este: %7.2f", Romberg(f, a, b));

P5. Implementa?i adquad.
% Folosim aceea?i integral?, de mai sus.
f = @(x)sin(x);
a = 0;
b = pi;
fprintf("Adquad:");
fprintf("Valoarea integralei date este: %7.2f", adquad(f, a, b, 1E-3, @Romberg));
fprintf("Adquad II:");
fprintf("Valoarea integralei date este: %7.2f", adquad2(f, a, b, 1E-3));
