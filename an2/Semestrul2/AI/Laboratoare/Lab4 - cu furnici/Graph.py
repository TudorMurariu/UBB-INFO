class Graph:
    def __init__(self, matrix, coord, no_of_nodes):
        self.__matrix = matrix
        self.__coord = coord
        self.__no_of_nodes = no_of_nodes
        self.__pheromone = [[1 / (no_of_nodes * no_of_nodes) for _ in range(no_of_nodes)] for _ in range(no_of_nodes)]

    @property
    def matrix(self):
        return self.__matrix

    @property
    def coord(self):
        return self.__coord

    @property
    def no_of_nodes(self):
        return self.__no_of_nodes

    @property
    def pheromone(self):
        return self.__pheromone
