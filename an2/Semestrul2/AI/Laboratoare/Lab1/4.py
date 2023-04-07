'''
    Să se determine cuvintele unui text care apar exact o 
    singură dată în acel text. De ex. cuvintele care apar o 
    singură dată în ”ana are ana are mere rosii ana" sunt: 
    'mere' și 'rosii'.
'''

# Time complexity: O(n)
# Space complexity: O(n)         -> we use a hashmap(dictionary)
def cuvinte_duplicate(words):
    wordsMap = { 1 } # a set
    for word in words.split(" "):
        if word in wordsMap:
            return word
        else: 
            wordsMap.add(word)
    return ""
    
def test():
    assert(cuvinte_duplicate("ana are ana are mere rosii ana"))
    assert(cuvinte_duplicate("Andrei merge la magazin la"))
    assert(cuvinte_duplicate("A B C D D E F"))


if __name__ == '__main__':
    test()