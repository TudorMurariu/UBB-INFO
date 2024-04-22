function P6_2
%P6_2
clf
pc =[
   -0.4   -0.6   -0.2    0.0    0.0    0.2    0.6  0.4
    0.6   -0.4   -0.6    0.0    0.0   -0.6   -0.4  0.6
    ];
npd=200;
q=gen_B_spline(pc,3,npd);
plot(q(1,:),q(2,:),'r-');
hold on
q1=gen_B_spline(pc,2,npd);
plot(q1(1,:),q1(2,:),'g-');
legend('grad=3','grad=2',0)
hold off
pause
clf
pc=[
   0   0   0   3   3   1.5  1.5   3   3  1  0.5
   -2  5   5   5   3   2.5  2.5   2   0  0  0.5
   ];
q=gen_B_spline(pc,3,npd);
plot(q(1,:),q(2,:),'r-');
hold on
q1=gen_B_spline(pc,2,npd);
plot(q1(1,:),q1(2,:),'g-');
legend('grad=3','grad=2',0)
hold off
function q=gen_B_spline(pc,g,npd)
n=length(pc);
kn=noduri(n-1,g);
ln=length(kn); 
pas=kn(ln)/npd;
t=0:pas:kn(ln);
q=Cox_deBoor(g,pc,kn,t);

