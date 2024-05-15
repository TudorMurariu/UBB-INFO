%parametric spline
%litera d

p=[
   0.25, 0.61;
   0.15,0.1;
   0,0.15;
   0.15,0.2;
   0.45,0.1;
   0.6, 0.35;
   0.4, 0.63;
   0.05,0.63;]
f=p(:,1);
g=p(:,2);

t=1:length(f);
sf=spline(t,f,1:0.1:length(t));
sg=spline(t,g,1:0.1:length(t));
plot(f,g,'o')
hold on
plot(sf,sg)
grid on
axis equal
xlabel('f')
ylabel('g')