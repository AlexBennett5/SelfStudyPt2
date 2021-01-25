import math

inf = math.inf
Edge = namedtuple('Edge', ['start', 'end', 'cost'])

class Graph:

    def __init__(self, edges):
        self.edges = [Edge(*edge) for edge in edges]
        self.vertices = {e.start for e in self.edges} | {e.end for e in self.edges}

    def dijkstra(self, begin, end):
        if begin not in self.vertices:
            raise RuntimeError("Invalid beginning vertex")
        dist = {vertex:inf for vertex in self.vertices}
        previous = {vertex:None for vertex in self.vertices}
        dist[begin] = 0
        q = self.vertices.copy()
        neighbors = {vertex:set() for vertex in self.vertices}

        for start, end, cost in self.edges:
            neighbors[start].add((end, cost))

        while q:
            u = min(q, key=lambda vertex: dist[vertex])
            q.remove(u)
            if dist[u] == inf or u == end:
                break
            for v, cost in neighbors[u]:
                alt = dist[u] + cost
                if alt < dist[v]:
                    dist[v] = alt
                    previous[v] = u
        s, u = deque(), dest
        while previous[u]:
            s.appendleft(u)
            u = previous[u]
        s.appendleft(u)



