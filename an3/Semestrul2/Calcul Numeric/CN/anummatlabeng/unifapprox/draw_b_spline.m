function draw_b_spline(pc,g,npd,stil)
%draw_b_spline(pc,grad,npd,stil)
%pc - punctele de control
%g - gradul
%npd - numar de puncte(rezolutia)
%stil - stilul de linie(conform sintaxei plot implicit '-')
n=length(pc);
kn=noduri(n-1,g);
ln=length(kn);
if nargin<2
   error('numar ilegal de argumente');
else
   if nargin==2
      npd=100;
      stil='-';
   else
      if nargin==3
         stil='r-';
      end
   end
end
pas=kn(ln)/npd;
t=0:pas:kn(ln);
q=Cox_deBoor(g,pc,kn,t);
plot(pc(1,:),pc(2,:),'--o',q(1,:),q(2,:),stil);