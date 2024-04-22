function P1_10
%P1_10 recursive subdivision of a triangle
%over midpoints

%x,y - vertex coordinates
%lx,ly - point coordinates (output)
%niv - subdivision level
[x,y]=ginput(3);
niv=input('subdivision level: ');
[lx,ly]=subdivision(x,y,niv);
T=delaunay(lx,ly);
triplot(T,lx,ly)