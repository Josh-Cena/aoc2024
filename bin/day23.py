import networkx as nx
import matplotlib.pyplot as plt

data = open("inputs/day23/real.txt").read().split("\n")
G = nx.Graph()
for line in data:
    u, v = line.split("-")
    G.add_edge(u, v)
plt.figure(figsize=(8, 10))
nx.draw(G, node_size=0, pos=nx.spring_layout(G, seed=42))
plt.savefig("bin/day23.png")
