%P5_9 inversare matrice Vandermonde
x=1:5; 
V=vander(x);
U=invvandermonde(x);
U*V
V*U
    