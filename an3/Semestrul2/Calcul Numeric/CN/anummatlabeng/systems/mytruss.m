% MYTRUSS   Solution to the truss problem.

n = 13;
A = zeros(n,n);
b = zeros(n,1);
alpha = 1/sqrt(2);

% Joint 2: f2 = f6
%          f3 = 10
A(1,2) = 1;
A(1,6) = -1;
A(2,3) = 1;
b(2) = 10;

% Joint 3: alpha f1 = f4 + alpha f5
%          alpha f1 + f3 + alpha f5 = 0
A(3,1) = alpha;
A(3,4) = -1;
A(3,5) = -alpha;
A(4,1) = alpha;
A(4,3) = 1;
A(4,5) = alpha;

% Joint 4: f4 = f8
%          f7 = 0
A(5,4) = 1;
A(5,8) = -1;
A(6,7) = 1;
b(6) = 0;

% Joint 5: alpha f5 + f6 = alpha f9 + f10
%          alpha f5 + f7 + alpha f9 = 15
A(7,5) = alpha;
A(7,6) = 1;
A(7,9) = -alpha;
A(7,10) = -1;
A(8,5) = alpha;
A(8,7) = 1;
A(8,9) = alpha;
b(8) = 15;

% Joint 6: f10 = f13
%          f11 = 20
A(9,10) = 1;
A(9,13) = -1;
A(10,11) = 1;
b(10) = 20;

% Joint 7: f8 + alpha f9 = alpha f12
%          alpha f9 + f11 + alpha f12 = 0
A(11,8) = 1;
A(11,9) = alpha;
A(11,12) = -alpha;
A(12,9) = alpha;
A(12,11) = 1;
A(12,12) = alpha;
b(12) = 0;

% Joint 8: f13 + alpha f12 = 0
A(13,13) = 1;
A(13,12) = alpha;
b(13) = 0;

x = A\b

clf reset
r = .15;
c = r*exp(2*pi*i*(0:32)/32);
axis([0 6 0 4])
axis equal
axis off
hold on
plot(c+(1+2i),'k-')
plot(c+(2+2i),'k-')
plot(c+(3+2i),'k-')
plot(c+(4+2i),'k-')
plot(c+(5+2i),'k-')
plot(c+(2+3i),'k-')
plot(c+(3+3i),'k-')
plot(c+(4+3i),'k-')

s = r/sqrt(2);
plot([1+s 2-s],[2+s 3-s],'k-')
plot([3+s 4-s],[2+s 3-s],'k-')
plot([4+s 5-s],[3-s 2+s],'k-')
plot([2+s 3-s],[3-s 2+s],'k-')

plot([2 2],[2+r 3-r],'k-')
plot([3 3],[2+r 3-r],'k-')
plot([4 4],[2+r 3-r],'k-')

plot([1+r 2-r],[2 2],'k-')
plot([2+r 3-r],[2 2],'k-')
plot([3+r 4-r],[2 2],'k-')
plot([4+r 5-r],[2 2],'k-')
plot([2+r 3-r],[3 3],'k-')
plot([3+r 4-r],[3 3],'k-')

text(1.6-.05,2.5,num2str(x(1),3))
text(2.1-.05,2.5,num2str(x(3),3))
text(2.6-.05,2.5,num2str(x(5),3))
text(3.1-.05,2.5,num2str(x(7),3))
text(3.6-.05,2.5,num2str(x(9),3))
text(4.1-.05,2.5,num2str(x(11),3))
text(4.6-.05,2.5,num2str(x(12),3))
text(1.5-.05,1.9,num2str(x(2),3))
text(2.5-.05,1.9,num2str(x(6),3))
text(3.5-.05,1.9,num2str(x(10),3))
text(4.5-.05,1.9,num2str(x(13),3))
text(2.5-.05,3.1,num2str(x(4),3))
text(3.5-.05,3.1,num2str(x(8),3))

plot([2,2],[2-r,1.25],'k-')
plot([3,3],[2-r,1.25],'k-')
plot([4,4],[2-r,1.25],'k-')

a = [0 .05+.2i -.05+.2i 0];
plot(a+(2+1.25i),'k-')
plot(a+(3+1.25i),'k-')
plot(a+(4+1.25i),'k-')

text(1.9,1.1,'10')
text(2.9,1.1,'15')
text(3.9,1.1,'20')
