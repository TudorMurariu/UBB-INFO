%P1_13 Problem 1.13
%perimeter and area of a square
x=[0,1,1,0];
y=[0,0,1,1];
p1=polyperim(x,y)
A1=polyarea(x,y)
%perimeter and area of an equilateral triangle
x=cos(2*[0:2]*pi/3);
y=sin(2*[0:2]*pi/3);
p2=polyperim(x,y)
A2=polyarea(x,y)