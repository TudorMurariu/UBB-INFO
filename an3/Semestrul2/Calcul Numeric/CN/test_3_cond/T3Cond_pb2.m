#Test 3 Cond, problema 2
# Sa se perturbe coeficientii lui f cu numere aleatoare avand distributia 
# N(0,10^(5)) si sa se reprezinte grafic radacinile.   
# Repetati de un numar mare de ori (de exemplu, n=1000) si pastrati pe ecran 
# punctele reprezentate la fiecare repetare.
clear;
root=2/3;
order=3;
coefs=poly(ones(1,order)*(root));
# perturbam coeficientii polinomului

clf; hold on; grid on;
#desenam radacinile originale
perturbed_roots=roots(coefs);
h=plot(real(perturbed_roots),imag(perturbed_roots),'*');
set(h,'Markersize',8);

# iteram de n=1000 ori pentru a evalua efectul perturbarilor
for k=1:1000
  new_coefs=coefs.*(1+normrnd(0,1e-5,1,length(coefs)));
  new_roots=roots(new_coefs);
  h2=plot(real(new_roots),imag(new_roots),'*');
  set(h2,'Markersize',6)
endfor
axis equal
  
