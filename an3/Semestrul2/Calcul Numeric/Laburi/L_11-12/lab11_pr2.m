format long

[nods,coeff] = Gauss_Legendre(10)
vi = vquad(nods,coeff,@functiaMagica)
Q = integral(@functiaMagica,-1,1)

errPatrat = (Q-vi)^2

function y = functiaMagica(x)
   y = sin(x.^2);
end



function I = vquad(g_nodes,g_coeff,f)
    I=g_coeff*f(g_nodes);
end