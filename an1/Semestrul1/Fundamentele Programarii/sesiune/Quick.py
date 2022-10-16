def QuickSort(list):
    if len(list) <= 1:
        return list

    pivot = list.pop()
    lesser = QuickSort([x for x in list if x < pivot])
    greater = QuickSort([x for x in list if x >= pivot])
    return lesser + [pivot] + greater


def test():
    l1 = [9,9,9,0,9,8,8,8,8,1,5,-9,-1,-3,-4]
    print(QuickSort(l1))

test()