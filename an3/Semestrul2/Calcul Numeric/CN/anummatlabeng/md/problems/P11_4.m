%P11_4
Bx=[0,3,6,9; 0,3,6,9; 0,3,6,9; 0,3,6,9];
By=[0,1,1,0; 2,2,3,2; 4,4,4,4; 6,6,5,6];
Bz=3*rand(4,4);
close all
gx=2; gy=2; nx=20; ny=20;
draw_Bspline_surf(Bx,By,Bz,gx,gy,nx,ny)