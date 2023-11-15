import re
from BST import BinarySearchTree

def getTS(cod: str, keywords : dict):
    # we get the variables by using regex
    # \s is for whitespaces
    pattern_variables = r'\b(float|int|string)\s+(\w+)\b'

    variables = re.findall(pattern_variables, cod)
    variables = [var[1] for var in variables]

    exception = None

    # remove reserved keywords from the variable list
    for var in variables:
        if var in keywords.keys():
            variables.remove(var)
        elif len(var) > 8:
            variables.remove(var)
            exception = Exception("A variable must have less than 9 characters")

    ts = BinarySearchTree()
    for var in variables:
        ts.insert(var)


    # the constants can be real numbers or strings
    pattern_constants = r'(\+|-|=|<|>|==|<=|>=|!=|\*|%|\\)\s+(\d+\.*\d*)'
    pattern_strings = r'(=|==)\s+"([^"]*)"'

    constants = re.findall(pattern_constants, cod)
    constants = [c[1] for c in constants]

    strings = re.findall(pattern_strings, cod)
    strings = [s[1] for s in strings]

    constants = constants + strings

    # remove reserved keywords from the constants list
    for c in constants:
        if c in keywords.keys():
            constants.remove(c)
        elif c in variables:
            constants.remove(c)

    constants = set(constants)

    for c in constants:
        ts.insert(c)

    return ts, exception