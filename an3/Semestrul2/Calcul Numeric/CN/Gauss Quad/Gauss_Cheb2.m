function [g_nodes,g_coeff]=Gauss_Cheb2(n)
%GAUSS_CHEB2 - Gauss-Chebyshev #2 nodes and coefficients

beta=[pi/2,1/4*ones(1,n-1)];
alpha=zeros(n,1);
[g_nodes,g_coeff]=Gaussquad(alpha,beta);