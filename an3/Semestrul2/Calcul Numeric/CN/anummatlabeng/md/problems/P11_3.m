%P11_3
close all
Bx=[0,2,4;0,2,4; 0,2,4];
By=[0,0,0; 2,2,2; 4,4,4];
Bz=[0,0,0;0,0,2;0,4,4];
nx=20; ny=20;
drawbeziersurf(Bx,By,Bz,nx,ny)
xlabel('x')
ylabel('y')
Bx=[0,2,4,6;0,2,4,6; 0,2,4,6; 0,2,4,6];
By=[0,0,0,0; 2,2,2,2; 4,4,4,4; 6,6,6,6];
Bz=[0,0,0,0;0,4,3,5;0,2,4,6; 0,5,6,5];
figure(2)
drawbeziersurf(Bx,By,Bz,nx,ny)
xlabel('x')
ylabel('y')