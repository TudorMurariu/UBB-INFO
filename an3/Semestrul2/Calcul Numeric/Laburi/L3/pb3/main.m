% Studiem conditionarea pentru (x-1)*(x-2)*...*(x-n) = 0
r = 1:20; % radacinile
poli1 = poly(r); % polinomul corespunzator
condpol(poli1, r)
figure(1)
nn=length(r);
h=plot(r,zeros(1,nn),'k.');
set(h,'Markersize',21);
hold on

% perturbatii
for i=1:20
    % distributie normala
    poli1_per_norm = poli1 + normrnd(0, 1e-10, 1, length(poli1));
    r_norm = roots(poli1_per_norm);
    h2=plot(r_norm,'b.');
    hold on
    set(h2,'Markersize',8)

    % distributie uniforma
    poli1_per_unif = poli1 + unifrnd(0, 1e-10, 1, length(poli1));
    r_unif = roots(poli1_per_unif);
    h3=plot(r_unif,'r*');
    hold on
    set(h3,'Markersize',4)
end
hold off


% Studiem conditionarea pentru a^k = 2^-k
poli2 = 2.^-(1:20); % polinomul corespunzator
condpol(poli2)
r2 = roots(poli2); % radacinile

figure(2)
nn=length(r2);
h=plot(r2,zeros(1,nn),'k.');
set(h,'Markersize',21);
hold on
for i=1:20
    % distributie normala
    poli1_per_norm = poli2 + normrnd(0, 1e-10, 1, length(poli2));
    r_norm = roots(poli1_per_norm);
    h2=plot(r_norm,'b.');
    hold on
    set(h2,'Markersize',8)

    % distributie uniforma
    poli1_per_unif = poli2 + unifrnd(0, 1e-10, 1, length(poli2));
    r_unif = roots(poli1_per_unif);
    h3=plot(r_unif,'r*');
    hold on
    set(h3,'Markersize',4)
end


hold off
