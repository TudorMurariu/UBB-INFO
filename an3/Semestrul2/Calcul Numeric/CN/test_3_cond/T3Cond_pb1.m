#Test 3 Cond, problema 1
# Fie polinomul f(x)=x^3-2x^2+4/3x-8/27.  
# Acest polinom are radacina tripla 2/3. 
# Determinati conditionarea acestei radacini.
root=2/3; # radacina pentru care se calculeaza conditionarea
order=3; # ordinul radacinii
cond=(1/root)*eps^(1/order-1)
# echivalent cu conda=eps^(-2/3)/root
# Concluzie: valoarea lui cond este foarte departe de 1 => radacina este foarte prost conditionata