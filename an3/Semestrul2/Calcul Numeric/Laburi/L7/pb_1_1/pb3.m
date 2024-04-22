timpi = [0, 3, 5, 8, 13];
distante = [0, 225, 383, 623, 993];
viteze = [75, 77, 80, 74, 72];
noduri = [10];

[d, dH] = Hermite_withDerivative(timpi, timpi, viteze, noduri);
fprintf("\nLa momentul t = 10, avem:");
fprintf("\nDistan?a: %f", d);
fprintf("\nViteza: %f", dH);