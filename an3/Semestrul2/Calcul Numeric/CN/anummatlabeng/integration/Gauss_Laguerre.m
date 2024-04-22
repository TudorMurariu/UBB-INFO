function [g_nodes,g_coeff]=Gauss_Laguerre(n,a)
%GAUSS_HERMITE - Gauss-Laguerre nodes and coefficients

k=1:n-1;
alpha=[a+1, 2*k+a+1];
beta=[gamma(1+a),k.*(k+a)];
[g_nodes,g_coeff]=Gaussquad(alpha,beta);