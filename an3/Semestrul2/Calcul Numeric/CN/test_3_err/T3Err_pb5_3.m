# Test 3 Err problema 5, subpunctul 3
# calculati termenii sirului {yn} pentru a=10 si n= 1,...,30, utilizand (2) repetat.
a=10;
# initializam matricele y, y_min si y_max cu zero si cu dimensiunea de 31x1
y=zeros(1,31);
y_min=zeros(1,31);
y_max=zeros(1,31);

# indexarea e de la 0 
# calculam primul termen al fiecarui vector folosind 
# relatia de recurenta si marginile inferioare si superioare rezultate
y(1)=log((a+1)/a); 
ymin(1)=1/(a+1);
ymax(1)=1/a;

for n=1:30
  y(n+1)=1/n-a*y(n);
  ymin(n+1)=1/((a+1)*(n+1));
  ymax(n+1)=1/(a*(n+1));
endfor
#afisam rezultatele obtinute
[y', ymin', ymax']
# Concluzie: valorile lui y nu sunt cuprinse in intervalul [ymin, ymax] => acuratete scazuta