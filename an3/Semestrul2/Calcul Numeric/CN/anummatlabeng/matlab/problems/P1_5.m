function p=P1_5(tab,a)
%P1_5 cautare binara intr-un tablou ordonat
%tab tabloul
%a - elementul
%p - pozitia in tablou, -1 in caz de esec

n=length(tab);
low=1;
high=n;
while(low<=high)
    mid=floor((low+high)/2);
    if tab(mid)==a
        p=mid;
        return;
    end
    if tab(mid)>a
        high=mid-1;
    else
        low=mid+1;
    end
end
%esec
p=-1;