function yder=f1scal(t,y)
%F1SCAL Exemple of scalar ODE
yder = -y+5*exp(-t).*cos(5*t);