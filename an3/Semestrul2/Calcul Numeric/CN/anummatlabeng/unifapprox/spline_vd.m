function [q,xi,t]=spline_vd(grad,d,f)
%SPLINE_VD Spline cu variatie diminuata
%apel [t,xi,q]=spline_vd(grad,d,f)
%grad - gradul
%d - diviziunea 
%f - functia
%q - ordonatele
%xi - punctele in care se evalueaza f
%t - abscisele

n=length(d)-grad-1;
xi=calcxi(d,n,grad);
pc=feval(f,xi);
t=min(d):(max(d)-min(d))/150:max(d);
q=Cox_deBoor(grad,pc,d,t);
%-----
function xi=calcxi(d,n,k)
%calculeaza mediile
for i=0:n-1
    xi(i+1)=sum(d(i+2:i+k+1))/k;
end