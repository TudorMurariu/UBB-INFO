function testRomberg()
    % test functie Romberg pt calculul unei Integrale
    % Vrem sa calculam Integrala de la 0 la pi din sin(x)dx, cu eps = 10^(-2)
    
    % f - ce integrala vrem sa calculam
    f = @(x) sin(x);
    
    % a, b - capele integralei
    a = 0;
    b = pi;
    
    epsi = 10e-2;
    
    % apelul functiei
    I = problema_4(f,a,b,epsi,50)
end
