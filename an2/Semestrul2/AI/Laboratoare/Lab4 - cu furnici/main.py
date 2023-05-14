import os
from math import sqrt
import time

import matplotlib.pyplot as plt

from ACO import ACO
from Graph import Graph
from plot import *


def read_graph_tsp(fileName):
    f = open(fileName, "r")
    line = f.readline()
    while "DIMENSION" not in line:
        line = f.readline()
    noNodes = int(line.split(': ')[1])
    while "NODE" not in line:
        line = f.readline()
    coord = []
    dictionary = {}
    for _ in range(noNodes):
        line = f.readline()
        elems = line.split(' ')
        dictionary[float(elems[0])] = [float(elems[1]), float(elems[2])]
        coord.append([float(elems[1]), float(elems[2])])
    f.close()
    matrix = []
    for i in dictionary.values():
        x1, y1 = i
        matrix.append([])
        for j in dictionary.values():
            x2, y2 = j
            matrix[-1].append(round(sqrt((x2 - x1) ** 2 + (y2 - y1) ** 2)))
    return {
        'noNodes': noNodes,
        'coord': coord,
        'matrix': matrix
    }


def plot_graph(coords, path=None):
    if path is None:
        path = []
    x = []
    y = []
    for coord in coords:
        x.append(coord[0])
        y.append(coord[1])
    plt.plot(x, y, 'b*', markersize=10)
    for i, j in zip(path + path[-1:], path[1:] + path[:1]):
        plt.arrow(x[i], y[i], x[j] - x[i], y[j] - y[i], color='g')
    plt.show()


def tests():
    print("EIL51")
    file = os.path.join(crt_dir, 'data', 'eil51.tsp')
    result = read_graph_tsp(file)
    aco_test = ACO(51, 30, 1.0, 10.0, 0.7, 3)
    graph_test = Graph(result['matrix'], result['coord'], result['noNodes'])
    # assert (aco_test.solve(graph_test)[1] - 426 <= 50)
    print("DJ38")
    file = os.path.join(crt_dir, 'data', 'dj38.tsp')
    result = read_graph_tsp(file)
    aco_test = ACO(38, 50, 1.0, 10.0, 0.7, 3)
    graph_test = Graph(result['matrix'], result['coord'], result['noNodes'])
    # assert (aco_test.solve(graph_test)[1] - 6656 <= 100)
    print("\nWI29")
    file = os.path.join(crt_dir, 'data', 'wi29.tsp')
    result = read_graph_tsp(file)
    aco_test = ACO(29, 50, 1.0, 10.0, 0.5, 3)
    graph_test = Graph(result['matrix'], result['coord'], result['noNodes'])
    # assert (aco_test.solve(graph_test)[1] - 27603 <= 2000)


if __name__ == '__main__':
    crt_dir = os.getcwd()
    # tests()
    #file_name = input('Give the name of the TSP file: ')
    file_name = "berlin52"
    file_name += ".tsp"
    file_path = os.path.join(crt_dir, 'data', file_name)
    cities = read_graph_tsp(file_path)

    # ants = int(input('Number of ants: '))
    # iterations = int(input('Number of iterations: '))
    # alpha = float(input('Alpha: '))
    # beta = float(input('Beta: '))
    # rho = float(input('Pheromone residual coefficient (between 0 and 1): '))
    # q = int(input('Pheromone intensity (Q): '))
    # dynamic = bool(int(input('Dynamic? True = 1/False = 0: ')))
    ants = 20
    iterations = 20
    alpha = 1 # lungime efectiv
    beta = 1 # calitate
    rho = 0.5 # nivelul de vaporizare (intre 0 si 1)
    q = 1
    dynamic = 1

    stTime = time.time()

    aco = ACO(ants, iterations, alpha, beta, rho, q, dynamic)
    graph = Graph(cities['matrix'], cities['coord'], cities['noNodes'])

    plotParam = aco.solve(graph)

    timeSpent = time.time() - stTime
    print("--- TOTAL %s seconds ---" %(timeSpent))

    print('\nCost:', plotParam["best_cost"], '\n Path:', plotParam["best_path"])
    printAndSavePlot(plotParam, timeSpent)
    plot_graph(cities['coord'], plotParam["best_path"])
    
