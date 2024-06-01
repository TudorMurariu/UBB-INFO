[nods,coeff] = Gauss_Hermite(10);

vquad(nods,coeff,@fOne)

function y = fOne(x)
   y = (exp(1).^(-x.^2)).*sin(x);
end

function y = fTwo(x)
   y = (exp(1).^(-x.^2)).*cos(x);
end


function I = vquad(g_nodes,g_coeff,f)
    I=g_coeff*f(g_nodes);
end