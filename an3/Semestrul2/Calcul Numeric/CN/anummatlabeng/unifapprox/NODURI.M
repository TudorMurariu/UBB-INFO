function kn=noduri(n,grad)
% obtine noduri multiple pentru B-spline
%apel  kn=noduri(n,grad)
%n - numarul de puncte de control
%grad - gradul spline-ului=multiplicitatea extremitatilor
kn=zeros(1,grad+1);
kn=[kn,1:n-grad];
kn=[kn,(n-grad+1)*ones(1,grad+1)];
%for i=n+1:n+grad+1
%  kn=[kn,n-grad+1];
%end
