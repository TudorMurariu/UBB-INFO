% functia x^2 pe intervalul [-2, 2]
x = [-2 2];
f = [4 4]; 
fd = [-4 4];

t = -2 : 0.01 : 2;
res = hermite(x, f, fd, t);

plot(t, res, 'Color', 'green');
