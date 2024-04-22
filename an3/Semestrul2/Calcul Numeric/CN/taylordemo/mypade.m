function R = mypade(f,m,k,x)
%  MYPADE Compute Pade approximation
% 
% call |R=mypade(f,m,k,x)|
syms C a R bt vp
c=zeros(m+k+1,1,'sym');
if m>k
    b=zeros(m+1,1,'sym');
else
    b=zeros(k+1,1,'sym');
end
a=zeros(1,m+1,'sym');
%% 
% Compute Taylor coefficients $c_k=\frac{f^{(k)}(0)}{k!}$
for p=0:m+k
    c(p+1)=subs(diff(f,x,p),x,0)/factorial(p);
end
%% 
% Build Toeplitz matrix and solve the system $\left\lbrack \begin{array}{ccccc}c_m  
% & c_{m-1}  & \cdots  & c_{m-\left(k-2\right)}  & c_{m-\left(k-1\right)} \\c_{m+1}  
% & c_m  & c_{m-1}  & c_{m-\left(k-3\right)}  & c_{m-\left(k-2\right)} \\\vdots  
% & \vdots  & \ddots  & \vdots  & \vdots \\c_{m+\left(k-2\right)}  & c_{m+\left(k-3\right)}  
% & \cdots  & c_m  & c_{m-1} \\c_{m+\left(k-1\right)}  & c_{m+\left(k-2\right)}  
% & \cdots  & c_{m+1}  & c_m \end{array}\right\rbrack \left\lbrack \begin{array}{c}b_1 
% \\b_2 \\\vdots \\b_{k-1} \\b_k \end{array}\right\rbrack =\left\lbrack \begin{array}{c}-c_{m+1} 
% \\-c_{m+2} \\\vdots \\-c_{m+\left(k-1\right)} \\-c_{m+k} \end{array}\right\rbrack$
j=0:k-1;
C=toeplitz(c(m+j+1),c(m-j+1));
bv=-c(m+(1:k)+1);
b(2:k+1)=C\bv;
b(1)=1;
%% 
% Compute $a_j =\sum_{\ell =0}^j c_{j-\ell } b_{\ell }$
syms j
for j=0:m
    a(j+1)=sym('0');
    for l=0:j
        a(j+1)=a(j+1)+c(j-l+1)*b(l+1); 
    end
end
vp=sum(a.*x.^(0:m));
vq=sum(b(1:k+1)'.*x.^(0:k));
R=vp/vq;
end