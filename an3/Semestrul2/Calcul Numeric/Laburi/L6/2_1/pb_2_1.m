addpath('../1_5');

% Pentru 1975:
x = [1900 1910 1920 1930 1940 1950 1960 1970 1980];
y = [75995 91972 105711 123203 131669 150697 179323 203212 226505];

fprintf("Popula?ia SUA în anul 1975:");
fprintf("%f", interpolare_Lagrange_baricentrica(x, y, 1975));

% Pentru 2018:
x = [1990 2000 2010];
y = [249633 281422 308786];

fprintf("Popula?ia SUA în anul 2018:");
fprintf("%f", interpolare_Lagrange_baricentrica(x, y, 2018));