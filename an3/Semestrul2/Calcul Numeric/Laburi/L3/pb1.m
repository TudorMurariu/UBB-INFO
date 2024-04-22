function pb1()

    % ; final de linie
    A = [10 7 8 7; 7 5 6 5; 8 6 10 9; 7 5 9 10]
    b = [32; 23; 33; 31]

    xe = A\b
    inv(A)
    format short
    xe = A\b
    inv(A)
    det(A)

    % a)
    disp('A)');
    bp = [32.1; 22.9; 33.1; 30.9] % bp- b perturbat
    xep = A\bp

    eri1 = norm(b-bp)/norm(b) % -> eroare la intrare
    ero1 = norm(xep-xe)/norm(xe) % -> eroare la iesire
    ero1/eri1 % -> raportul erorilor

    cond(A) % afisati nr de conditionare a matricii A, norma p=1

    % b)
    disp('B)');
    Ap = [10 7 8.1 7.2; 7.8 5.04 6 5; 8 5.98 9.89 9; 6.99 4.99 9 9.98];

    y = A\b; % \ - Matrix left divison
    yp = Ap\b;

    er1 = norm((A-Ap),1)/norm(A,1)
    er2 = norm((y-yp),1)/norm(y,1)
    er2/er1

    cond(A,1) % afisati nr de conditionare a matricii A, norma p=1
end
