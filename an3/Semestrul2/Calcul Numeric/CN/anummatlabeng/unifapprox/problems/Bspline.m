function y=Bspline(g,t,i,tt)
%Bspline - calculul unei functii B-spline
%apel y=Bspline(g,t,i,tt)
%g - gradul g>=0
%t - nodurile
%i - indicele B-spline (0:n-1)
%tt - punctele in care se calculeaza valoarea

m=length(t)-1;
n=m-g;
pc=[zeros(1,i),1,zeros(1,n-i-1)];
y=Cox_deBoor(g,pc,t,tt);