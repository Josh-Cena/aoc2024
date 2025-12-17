import networkx as nx

data = open("inputs/day23/real.txt").read().split("\n")
G = nx.Graph()
for line in data:
    u, v = line.split("-")
    G.add_edge(u, v)

total = 0
for t in nx.all_triangles(G):
    if t[0].startswith("t") or t[1].startswith("t") or t[2].startswith("t"):
        total += 1
print(total)

print(",".join(sorted(nx.max_weight_clique(G, None)[0])))
