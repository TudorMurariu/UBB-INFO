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

def test_all():
    # testele de la profa
    # test_dolphins()
    # test_football()
    # test_karate()
    # test_krebs()

    # teste in plus
    # test_lesmis()
    # test_polbooks()
    # test_adjnoun()
    test_netscience()
    # test_cond_mat()
    # test_cond_mat_2003()

if __name__ == '__main__':
    test_all()