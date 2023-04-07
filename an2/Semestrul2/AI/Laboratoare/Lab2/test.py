from main import *

def test_dolphins():
    crtDir =  os.getcwd()
    filePath = os.path.join(crtDir,  'real', 'dolphins', 'dolphins.gml')
    Graph = nx.read_gml(filePath, label='id')
    G_copy = Graph.copy()
    plotNetwork(G_copy, greedyCommunitiesDetectionByTool(Graph, 2))

def test_football():
    crtDir =  os.getcwd()
    filePath = os.path.join(crtDir,  'real', 'football', 'football.gml')
    Graph = nx.read_gml(filePath, label='id')
    G_copy = Graph.copy()
    plotNetwork(G_copy, greedyCommunitiesDetectionByTool(Graph, 12))

def test_karate():
    crtDir =  os.getcwd()
    filePath = os.path.join(crtDir,  'real', 'karate', 'karate.gml')
    Graph = nx.read_gml(filePath, label='id')
    G_copy = Graph.copy()
    plotNetwork(G_copy, greedyCommunitiesDetectionByTool(Graph, 2))

def test_krebs():
    crtDir =  os.getcwd()
    filePath = os.path.join(crtDir,  'real', 'krebs', 'krebs.gml')
    Graph = nx.read_gml(filePath, label='id')
    G_copy = Graph.copy()
    plotNetwork(G_copy, greedyCommunitiesDetectionByTool(Graph, 3))   

def test_as_22july06():
    crtDir =  os.getcwd()
    filePath = os.path.join(crtDir,  'real', 'as-22july06', 'as-22july06.gml')
    Graph = nx.read_gml(filePath, label='id')
    G_copy = Graph.copy()
    plotNetwork(G_copy, greedyCommunitiesDetectionByTool(Graph, 2))    

def test_cond_mat():
    crtDir =  os.getcwd()
    filePath = os.path.join(crtDir,  'real', 'cond-mat', 'cond-mat.gml')
    Graph = nx.read_gml(filePath, label='id')
    G_copy = Graph.copy()
    plotNetwork(G_copy, greedyCommunitiesDetectionByTool(Graph, 2))    

def test_netscience():
    crtDir =  os.getcwd()
    filePath = os.path.join(crtDir,  'real', 'netscience', 'netscience.gml')
    Graph = nx.read_gml(filePath, label='id')
    G_copy = Graph.copy()
    plotNetwork(G_copy, greedyCommunitiesDetectionByTool(Graph, 900)) 

def test_polbooks():
    crtDir =  os.getcwd()
    filePath = os.path.join(crtDir,  'real', 'polbooks', 'polbooks.gml')
    Graph = nx.read_gml(filePath, label='id')
    G_copy = Graph.copy()
    plotNetwork(G_copy, greedyCommunitiesDetectionByTool(Graph, 4))      

def test_adjnoun():
    crtDir =  os.getcwd()
    filePath = os.path.join(crtDir,  'real', 'adjnoun', 'adjnoun.gml')
    Graph = nx.read_gml(filePath, label='id')
    G_copy = Graph.copy()
    plotNetwork(G_copy, greedyCommunitiesDetectionByTool(Graph, 2))  

def test_lesmis():
    crtDir =  os.getcwd()
    filePath = os.path.join(crtDir,  'real', 'lesmis', 'lesmis.gml')
    Graph = nx.read_gml(filePath, label='id')
    G_copy = Graph.copy()
    plotNetwork(G_copy, greedyCommunitiesDetectionByTool(Graph, 3))  

def test_star():
    crtDir =  os.getcwd()
    filePath = os.path.join(crtDir,  'real', 'star', 'star.gml')
    Graph = nx.read_gml(filePath, label='id')
    G_copy = Graph.copy()
    plotNetwork(G_copy, greedyCommunitiesDetectionByTool(Graph, 2))  

def test_lobster():
    crtDir =  os.getcwd()
    filePath = os.path.join(crtDir,  'real', 'lobster', 'lobster.gml')
    Graph = nx.read_gml(filePath, label='id')
    G_copy = Graph.copy()
    plotNetwork(G_copy, greedyCommunitiesDetectionByTool(Graph, 4))  

def test_shell():
    crtDir =  os.getcwd()
    filePath = os.path.join(crtDir,  'real', 'shell', 'shell.gml')
    Graph = nx.read_gml(filePath, label='id')
    G_copy = Graph.copy()
    plotNetwork(G_copy, greedyCommunitiesDetectionByTool(Graph, 2))  

def test_all():
    # testele de la profa
    # test_dolphins()
    # test_football()
    # test_karate()
    # test_krebs()

    # teste in plus
    test_lesmis()
    test_polbooks()
    test_adjnoun()
    test_star()
    test_lobster()
    test_shell()

if __name__ == '__main__':
    test_all()
    # stest_all()
    # constructor = [(10, 20, 0.8), (20, 40, 0.8)]
    # G = nx.random_shell_graph(constructor)
    # pos = nx.spring_layout(G) 
    
    # nx.draw_networkx_nodes(G, pos, node_size=600, cmap=plt.cm.RdYlBu)
    # nx.draw_networkx_edges(G, pos, alpha=0.3)
    # nx.draw_networkx_labels(G, pos)
    # plt.show()

    # nx.write_gml(G, 'real\shell\shell.gml')

