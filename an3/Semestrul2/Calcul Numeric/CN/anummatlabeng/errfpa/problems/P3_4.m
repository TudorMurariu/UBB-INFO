%P3_4
F=zeros(1,2000);
F(1)=1; F(2)=1; i=2;
ndep=true;
while F(i)<=realmax
    i=i+1;
    F(i)=F(i-1)+F(i-2);
    % [i,F(i)]
    if F(i)>2^53 & ndep
        disp('ultimul exact')
        [int64(i),int64(F(i))]
        ndep=false;
    end
end
i-1
    