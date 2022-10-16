def Sort(list1,start,end):
    if end-1 != start:
        mid = (start+end)//2
        Sort(list1,start,mid)
        Sort(list1,mid,end)
        Merge(list1,start,mid,end)

def Merge(list1,start,mid,end):
    i = start
    j = mid
    c = []

    while i < mid and j < end:
        if list1[i] < list1[j]:
            c.append(list1[i])
            i += 1
        else:
            c.append(list1[j])
            j += 1

    while i < mid:
        c.append(list1[i])
        i += 1

    while j < end:
        c.append(list1[j])
        j += 1

    for i in range(len(c)):
        list1[start + i] = c[i]



def test():
    l3 = [5,4,8,8,2,3]
    Sort(l3,0,len(l3))
    print(l3)

    l1 = [1,5,6,3,2]
    Sort(l1,0,len(l1))
    assert l1 == [1,2,3,5,6]
    l2 = [7,7,8,1,1,2,1,-3]
    Sort(l2,0,len(l2))
    assert l2 == [-3,1,1,1,2,7,7,8]
    

test()