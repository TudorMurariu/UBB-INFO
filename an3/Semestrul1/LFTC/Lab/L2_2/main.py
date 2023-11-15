from model.specs import *
from model.FIP import ProgramInternalForm
from model.scanner import tokenGenerator, isIdentifier, isConstant, isIntegerAF, isRealAF, isIdentifierAF
from model.symbol_table import SymbolTable

if __name__ == '__main__':
    fileName = 'examples/' + input("Insert file name: ")


    with open(fileName, 'r', encoding='utf-8') as file:
        for line in file:
            print([token for token in tokenGenerator(line, separators)])

    symbolTable = [SymbolTable(), SymbolTable(), SymbolTable()] #  0 pentru identificatori, 1 pt int, 2 pt real
    pif = ProgramInternalForm()

    with open(fileName, 'r', encoding='utf-8') as file:
        lineNo = 0
        for line in file:
            lineNo += 1
            for token in tokenGenerator(line[0:-1], separators):
                if token in separators + operators + reserved_words:
                    pif.add(codification[token], -1)
                #elif isIdentifier(token):
                elif isIdentifierAF(token):
                    id = symbolTable[0].add(token)
                    pif.add(codification['identifier'], id)
                # elif isConstant(token):
                #     id = symbolTable[1].add(token)
                #     pif.add(codification['constant'], id)
                elif isIntegerAF(token):
                    id = symbolTable[1].add(token)
                    pif.add(codification['integer_constant'], id)
                elif isRealAF(token):
                    id = symbolTable[2].add(token)
                    pif.add(codification['real_constant'], id)
                else: 
                    raise Exception('Unknown token ' + token + ' at line ' + str(lineNo))

    print()
    print('Program Internal Form: \n', pif)
    print()
    print('Symbol Table ( identificators ): \n', symbolTable[0])
    print()
    print('Symbol Table ( integer_constants ):  \n', symbolTable[1])
    print()
    print('Symbol Table ( real_constants ):  \n', symbolTable[2])
    # print()
    # print('Symbol Table ( constants ):  \n', symbolTable[1])
    print()
    print()
    print('Codification table: ')
    for e in codification:
        print(e, " -> ", codification[e])

