# Test 3 Err problema 5, subpunctul 4
# rezolvati (2) in raport cu yn_1 si calculati din nou sirul pentru a=10,
# de aceasta data n mergand in jos si incepand cu n=30.
# luati ca valoare de pornire marginea inferioara pentru y_30.
a=10;
# initializam matricele y, y_min si y_max cu zero si cu dimensiunea de 30x1
ymin=zeros(1,30);
y=zeros(1,30);
ymax=zeros(1,30);

# y(n)=y_(n-1) <-> indexarea e de la 0
# calculam termenii initiali y(30), y_min(30) si y_max(30) folosind 
# relatia de recurenta si marginea inferioara rezultata pentru y(30)
y(30)=1/((a+1)*31);
ymin(30)=1/((a+1)*31);
ymax(30)=1/(a*31);

for n=29:-1:1
  y(n)=(1-(n+1)*y(n+1))/(a*(n+1));
  ymin(n)=1/((a+1)*(n+1));
  ymax(n)=1/(a*(n+1));
endfor

# afisam rezultatele obtinute
[y', ymin', ymax']
# Concluzie: valorile lui y sunt cuprinse in intervalul [ymin, ymax] => acuratete mai buna