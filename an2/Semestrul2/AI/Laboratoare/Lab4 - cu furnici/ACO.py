from math import sqrt
from random import randint, random


class ACO:
    def __init__(self, no_of_ants, iterations, alpha, beta, rho, Q, dynamic=False):
        self.__no_of_ants = no_of_ants
        self.__iterations = iterations
        self.__alpha = alpha
        self.__beta = beta
        self.__rho = rho
        self.__Q = Q
        self.__dynamic = dynamic

    @property
    def no_of_ants(self):
        return self.__no_of_ants

    @property
    def iterations(self):
        return self.__iterations

    @property
    def alpha(self):
        return self.__alpha

    @property
    def beta(self):
        return self.__beta

    @property
    def rho(self):
        return self.__rho

    @property
    def Q(self):
        return self.__Q

    def __update_pheromone(self, graph, ants):
        for i in range(graph.no_of_nodes):
            for j in range(graph.no_of_nodes):
                graph.pheromone[i][j] *= (1 - self.rho)
                for ant in ants:
                    graph.pheromone[i][j] += ant.pheromone_delta[i][j]

    def __dynamic_graph(self, graph, best_path):
        to_change = randint(1, graph.no_of_nodes)
        print(to_change)
        for i in range(to_change):
            node = randint(0, graph.no_of_nodes - 1)
            print('\nNode', node)
            print('Old', graph.coord[node][0], graph.coord[node][1])
            x = randint(3 * int(graph.coord[node][0] // 4), int(graph.coord[node][0]))
            y = randint(3 * int(graph.coord[node][1] // 4), int(graph.coord[node][1]))
            while [x, y] in graph.coord:
                x = randint(3 * int(graph.coord[node][0] / 4), int(graph.coord[node][0]))
                y = randint(3 * int(graph.coord[node][1] / 4), int(graph.coord[node][1]))
            graph.coord[node][0] = x
            graph.coord[node][1] = y
            print('New', graph.coord[node][0], graph.coord[node][1])
            for city in range(graph.no_of_nodes):
                x1, y1 = graph.coord[node]
                x2, y2 = graph.coord[city]
                graph.matrix[node][city] = round(sqrt((x2 - x1) ** 2 + (y2 - y1) ** 2))
        cost = 0
        for i in range(len(best_path) - 1):
            cost += graph.matrix[best_path[i]][best_path[i + 1]]
        cost += graph.matrix[best_path[0]][best_path[-1]]
        return cost

    def solve(self, graph):
        best_cost = float('INF')
        best_path = []
        plotParam = { "best_path": best_path, "best_cost": best_cost, 'allBestFitnesses' : [], 'allWorstFitnesses' : [], 'allAvgFitnesses' : [], 'iteration': [] }
        
        for iteration in range(self.iterations):
            if iteration == self.iterations // 4 and self.__dynamic is True:
                best_cost = self.__dynamic_graph(graph, best_path)
            worst_gen_cost = float(0)
            best_gen_cost = float('INF')
            avg = 0.0
            ants = [Ant(self, graph) for _ in range(self.no_of_ants)]

            for ant in ants:
                for i in range(graph.no_of_nodes - 1):
                    ant.select_next()
                ant.total_cost += graph.matrix[ant.solution[-1]][ant.solution[0]]
                avg += ant.total_cost
                if ant.total_cost < best_cost:
                    best_cost = ant.total_cost
                    best_path = [] + ant.solution
                if ant.total_cost > worst_gen_cost:
                    worst_gen_cost = ant.total_cost
                if ant.total_cost < best_gen_cost:
                    best_gen_cost = ant.total_cost
                ant.update_pheromone_delta()
                
            plotParam['allBestFitnesses'].append(best_gen_cost)
            plotParam['allWorstFitnesses'].append(worst_gen_cost)
            plotParam['allAvgFitnesses'].append(avg/self.__no_of_ants)
            plotParam['iteration'].append(iteration)
            self.__update_pheromone(graph, ants)
            # print('\nGeneration:', iteration, '\nbest cost:', best_gen_cost)
        plotParam["best_path"] = best_path
        plotParam["best_cost"] = best_cost
        return plotParam




class Ant:
    def __init__(self, aco, graph):
        self.__colony = aco
        self.__graph = graph
        self.__total_cost = 0.0
        self.__solution = []
        self.__pheromone_delta = []
        self.__unvisited = [i for i in range(graph.no_of_nodes)]
        self.__eta = [[0 if i == j else 1 / graph.matrix[i][j] for j in range(graph.no_of_nodes)] for i in
                      range(graph.no_of_nodes)]
        start = randint(0, graph.no_of_nodes - 1)
        self.__current = start
        self.__solution.append(start)
        self.__unvisited.remove(start)

    @property
    def total_cost(self):
        return self.__total_cost

    @total_cost.setter
    def total_cost(self, value):
        self.__total_cost = value

    @property
    def solution(self):
        return self.__solution

    @property
    def pheromone_delta(self):
        return self.__pheromone_delta

    def select_next(self):
        lk = 0
        for i in self.__unvisited:
            lk += (self.__graph.pheromone[self.__current][i] ** self.__colony.alpha) * \
                           (self.__eta[self.__current][i] ** self.__colony.beta)
        probabilities = [0 for _ in range(self.__graph.no_of_nodes)]
        for i in range(self.__graph.no_of_nodes):
            if i in self.__unvisited:
                probabilities[i] = (self.__graph.pheromone[self.__current][i] ** self.__colony.alpha) * \
                                   (self.__eta[self.__current][i] ** self.__colony.beta) / lk
        selected = 0
        rand = random()
        for i in range(self.__graph.no_of_nodes):
            rand -= probabilities[i]
            if rand <= 0:
                selected = i
                break
        self.__unvisited.remove(selected)
        self.__solution.append(selected)
        self.__total_cost += self.__graph.matrix[self.__current][selected]
        self.__current = selected

    def update_pheromone_delta(self):
        self.__pheromone_delta = [[0 for _ in range(self.__graph.no_of_nodes)] for _ in range(self.__graph.no_of_nodes)]
        for k in range(1, len(self.__solution)):
            i = self.__solution[k - 1]
            j = self.__solution[k]
            self.__pheromone_delta[i][j] = self.__colony.Q / self.__total_cost
