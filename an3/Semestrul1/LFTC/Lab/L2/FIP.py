from BST import BinarySearchTree
from collections import OrderedDict

def getFIP(cod: str, keywords: dict, ts: BinarySearchTree):
    words = cod.split()
    fip = OrderedDict()
    exception = None
    
    for word in words:
        if word in fip.keys():
            continue
        elif word in keywords.keys():
            fip[word] = keywords[word]
        elif ts.get_index(word.replace('"', '')) != None:
            if word[0].isalpha():
                fip[word] = str(keywords['ID']) + ' | ' + str(ts.get_index(word))
            else:
                fip[word] = str(keywords['CONST']) + ' | ' + str(ts.get_index(word.replace('"', '')))
        else:
            exception = Exception("The word: " + word + " is neitehr a keyword , a variable or a constant.")
    
    return fip, exception