% MYTRUSS2   Solution to the truss problem.

n = 21;
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

% Joint 6: f10 = f14
%          f11 = 20
A(9,10) = 1;
A(9,14) = -1;
A(10,11) = 1;
b(10) = 20;

% Joint 7: f8 + alpha f9 = f12 +alpha f13
%          alpha f9 + f11 + alpha f13 = 0
A(11,8) = 1;
A(11,9) = alpha;
A(11,12) = -1;
A(11,13) = -alpha;
A(12,9) = alpha;
A(12,11) = 1;
A(12,13) = alpha;
b(12) = 0;

% Joint 8: f12=f16
%          f15=0
A(13,12) = 1;
A(13,16) = -1;
A(14,15) =1;
b(14) = 0;

% Joint 9: alpha f13 + f14 = alpha f17 +f18
%          alpha f13+f15+alpha f17 =25
A(15,13)=alpha;
A(15,14)=1;
A(15,17)=-alpha;
A(15,18)=-1;
A(16,13)=alpha;
A(16,15)=1;
A(16,17)=alpha;
b(16)=25;

%Joint 10: f18=f21;
%          f19=30;
A(17,18)=1;
A(17,21)=-1;
A(18,19)=1;
b(18)=30;

%Joint 11 f16+alpha f17=f20
%         alpha f17 +f19+alpha f20=0;
A(19,16)=1;
A(19,17)=alpha;
A(19,20)=-1;
A(20,17)=alpha;
A(20,19)=1;
A(20,20)=alpha;
b(20)=0;

% Joint 12 alpha f20+f21=0;

A(21,20)=alpha;
A(21,21)=1;


x = A\b;

clf reset
r = .15;
c = r*exp(2*pi*i*(0:32)/32);
axis([0 8 0 4])
axis equal
axis off
hold on
%plot joints
for k=1:7
    plot(c+(k+2i),'k-')
end
for k=2:6
    plot(c+(k+3i),'k-')
end

s = r/sqrt(2);
plot([1+s 2-s],[2+s 3-s],'k-')
plot([3+s 4-s],[2+s 3-s],'k-')
plot([5+s 6-s],[2+s 3-s],'k-')
plot([4+s 5-s],[3-s 2+s],'k-')
plot([2+s 3-s],[3-s 2+s],'k-')
plot([6+s 7-s],[3-s 2+s],'k-')

plot([2 2],[2+r 3-r],'k-')
plot([3 3],[2+r 3-r],'k-')
plot([4 4],[2+r 3-r],'k-')
plot([5 5],[2+r 3-r],'k-')
plot([6 6],[2+r 3-r],'k-')

plot([1+r 2-r],[2 2],'k-')
plot([2+r 3-r],[2 2],'k-')
plot([3+r 4-r],[2 2],'k-')
plot([4+r 5-r],[2 2],'k-')
plot([5+r 6-r],[2,2],'k-')
plot([6+r 7-r],[2,2],'k-') 
plot([2+r 3-r],[3 3],'k-')
plot([3+r 4-r],[3 3],'k-')
plot([4+r 5-r],[3 3],'k-')
plot([5+r 6-r],[3,3],'k-')

text(0.8-.05,2.5,num2str(x(1),3))
text(1.7-.05,2.5,num2str(x(3),3))
text(2.1-.05,2.4,num2str(x(5),3))
text(2.8-.05,2.5,num2str(x(7),3))
text(3.1-.05,2.7,num2str(x(9),3))
text(3.6-.05,2.4,num2str(x(11),3))
text(4.1-.05,2.3,num2str(x(13),3))
text(4.8-.05,2.5,num2str(x(15),3))
text(5.5-.05,2.3,num2str(x(17),3))
text(6.1-.05,2.5,num2str(x(19),3))
text(6.7-.05,2.5,num2str(x(20),3))

text(6.6-0.5,1.7,num2str(x(21),4),'HorizontalAlignment','left')
text(5.6-0.5,1.7,num2str(x(18),4),'HorizontalAlignment','left')
text(4.6-0.5,1.7,num2str(x(14),4),'HorizontalAlignment','left')
text(3.6-0.5,1.7,num2str(x(10),4),'HorizontalAlignment','left')
text(2.6-0.5,1.7,num2str(x(6),4),'HorizontalAlignment','left')
text(1.6-0.5,1.7,num2str(x(2),4),'HorizontalAlignment','left')

text(2.5-.05,3.2,num2str(x(4),4),'HorizontalAlignment','center')
text(3.5-.05,3.2,num2str(x(8),4),'HorizontalAlignment','center')
text(4.5-.05,3.2,num2str(x(12),4),'HorizontalAlignment','center')
text(5.5-.05,3.2,num2str(x(16),4),'HorizontalAlignment','center')

plot([2,2],[2-r,1.25],'k-')
plot([3,3],[2-r,1.25],'k-')
plot([4,4],[2-r,1.25],'k-')
plot([5,5],[2-r,1.25],'k-')
plot([6,6],[2-r,1.25],'k-')

a = [0 .05+.2i -.05+.2i 0];
plot(a+(2+1.25i),'k-')
plot(a+(3+1.25i),'k-')
plot(a+(4+1.25i),'k-')
plot(a+(5+1.25i),'k-')
plot(a+(6+1.25i),'k-')

text(1.9,1.1,'10')
text(2.9,1.1,'15')
text(3.9,1.1,'20')
text(4.9,1.1,'25')
text(5.9,1.1,'30')
