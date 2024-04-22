function [qx,qy,qz]=deCasteljau2D(Bx,By,Bz,u,v)
%DECASTELJAU2D - algoritmul de Casteljau bidimensional
%apel: [qx,qy,qz]=deCasteljau2D(Bx,By,Bz,u,v)
%Bx,By,Bz - matricele punctelor de control
%u,v - coordonate in care se face evaluarea
[n,m]=size(Bx);
lu=length(u); lv=length(v);
[U,V]=meshgrid(u,v);
for k=1:lu
    for p=1:lv
        %calculul pentru un punct
        rx=Bx; ry=By; rz=Bz;
        u=U(k,p); v=V(k,p);

        for j=1:m-1
            [rx,ry,rz]=C2(rx,ry,rz,v);
        end
        for i=1:n-1
            [rx,ry,rz]=C1(rx,ry,rz,u);
        end
        qx(k,p)=rx; qy(k,p)=ry; qz(k,p)=rz;
    end
end
function [X,Y,Z]=C2(MX,MY,MZ,t)
%operator de Casteljau
%L2 - inlatura prima coloana
%R2 - inlatura ultima coloana
%C2=(1-t)*R2+t*L2
X=(1-t)*MX(:,1:end-1)+t*MX(:,2:end);
Y=(1-t)*MY(:,1:end-1)+t*MY(:,2:end);
Z=(1-t)*MZ(:,1:end-1)+t*MZ(:,2:end);

function [X,Y,Z]=C1(MX,MY,MZ,t)
%operator de Casteljau
%L1 - inlatura prima linie
%R1 - inlatura ultima linie
%C1=(1-t)*R1+t*L1
X=(1-t)*MX(1:end-1,:)+t*MX(2:end,:);
Y=(1-t)*MY(1:end-1,:)+t*MY(2:end,:);
Z=(1-t)*MZ(1:end-1,:)+t*MZ(2:end,:);