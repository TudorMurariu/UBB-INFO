from model.lexicographic_table import LexicographicTable


class SymbolTable:
    def __init__(self):
        self.__sortedList = LexicographicTable()

    def add(self, value):
        return self.__sortedList.add(value)

    def get(self, value):
        return self.__sortedList.getId(value)

    def __str__(self):
        return str(self.__sortedList)
