function I = Romberg(f, a, b, err, nmax)
    % f - functia
    % a,b - interval
    % err - eroare maxima acceptata
    % nmax - numar maxim de iteratii
    
    if nargin < 5
      nmax=10;
    end
    if nargin < 4
      err=1e-6;
    end
    R=zeros(nmax,nmax);
    h=b-a;
    R(1,1)=h/2*(sum(f([a,b])));
    for k=2:nmax
       x=a+([1:2^(k-2)]-0.5)*h;
       R(k,1)=0.5*(R(k-1,1)+h*sum(f(x)));
       plj=4;
       for j=2:k
          R(k,j)=(plj*R(k,j-1)-R(k-1,j-1))/(plj-1);
          plj=plj*4;
       end
       if (abs(R(k,k)-R(k-1,k-1))<err)&&(k>3)
          I=R(k,k);
          return
       end
       h=h/2;
    end
end