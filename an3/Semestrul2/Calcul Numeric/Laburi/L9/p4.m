%p4
% Potrivire polinomial?:
y = [75995 91972 105710 123200 131670 150700 179320 203210 226510 249630 281420 308790]';
t = (1900:10:2010)';
x = (1890:1:2019)';
w = 1975;

fprintf("Potrivire polinomial?:");

s = (t - 1950) / 50;
xs = (x - 1950) / 50;
cs = polyfit(s, y, 3);
zs = polyval(cs, xs);
est = polyval(cs, (1975 - 1950) / 50);
plot(t, y, 'o', x, zs, '-', w, est, '*');
text(1990, est, num2str(est));
title("Popula?ia SUA în anul 1975", "FontSize", 14);
xlabel("Anul", "FontSize", 12);
ylabel("Milioane", "FontSize", 12);
%{
w = 2010;

s = (t - 1950) / 50;
xs = (x - 1950) / 50;
cs = polyfit(s, y, 3);
zs = polyval(cs, xs);
est = polyval(cs, (2010 - 1950) / 50);
plot(t, y, 'o', x, zs, '-', w, est, '*');
text(1990, est, num2str(est));
title("Popula?ia SUA în anul 2010", "FontSize", 14);
xlabel("Anul", "FontSize", 12);
ylabel("Milioane", "FontSize", 12);
%}