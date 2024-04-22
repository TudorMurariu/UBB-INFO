pl=[0;0]; %capatul stang
pr=[1;0]; %capatul drept

for k = 1:4
    subplot(2,2,k)
    koch(pl,pr,k)
    axis('equal')
    title(['Curba Koch: nivel = ', num2str(k)], 'FontSize', 16)
end
hold off
