function P11_7
f=@(x,y) y.*cos(x.^2);
c=@(x) zeros(size(x));
d=@(x) 1-x;
%punctul a - reprezentarea
x=[0,1,0]; y=[0,0,1]; niv=4;
[lx,ly]=subdivizare(x,y,niv);
tri = delaunay(lx,ly);
Z=f(lx,ly);
T=delaunay(lx,ly);
trisurf(tri,lx,ly,Z)
%punctul b - integrala pe patrat
vi1=dblquad(@intgr,0,1,0,1,1e-10,@quad,f)
%punctul c - integrala pe triunghi
vi2=quaddblsx(f,0,1,c,d,1e-10,@quadl)
function z=intgr(x,y,f)
%integrandul f(x,y) pe triunghi, zero inafara
for i=1:length(x)
    for j=1:length(y)
        if (x(i)>=0 & x(i)<=1 & y(j)<=1-x(i))
            z(i,j)=f(x(i),y(j));
        else
            z(i,j)=0;
        end
    end
end
