clf, axis([-2 2 -2 2]), axis equal
h = line(NaN,NaN,'marker','o','linestyle','-','erasemode','none');
t = 6*pi*(0:0.02:1);
for n = 1:length(t)
    set(h,'XData',2*cos(t(1:n)),'YData',sin(t(1:n)))
    pause(0.05)
end