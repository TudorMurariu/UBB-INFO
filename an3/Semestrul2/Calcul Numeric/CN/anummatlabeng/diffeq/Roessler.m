function yd=Roessler(t,y,a,b,c)
%ROESSLER sistemul Roessler parametrizat

yd = [-y(2)-y(3); y(1)+a*y(2); b+y(3)*(y(1)-c)];