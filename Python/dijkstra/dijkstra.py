import math
import pqueue

inf = math.inf

def dijkstra(graph, src, dest):
    unvisited = {v for v in graph.keys()}
    
    if src not in unvisited or dest not in unvisited:
        raise RuntimeError("Invalid vertex")
    
    dist = {v:inf for v in unvisited}
    dist[src] = 0
    prev = {}
    pq = pqueue.PriorityQueue()
    
    for v in unvisited:
        pq.push(dist[v], v)

    while unvisited:
        priority, v = pq.pop()
        unvisited.remove(v)
        if v == dest: break

        for u in graph[v]:
            if u in unvisited:
                if graph[v][u] < 0:
                    raise RuntimeError("Negative edge")
                
                alt = dist[v] + graph[v][u]
                if alt < dist[u]:
                    pq.push(alt, u)
                    dist[u] = alt
                    prev[u] = v

    return get_path(prev, src, dest), dist[dest] 
    
def get_path(prev, src, dest):
    v = dest
    path = [v]
    while v != src:
        v = prev[v]
        path.append(v)
    path.reverse()
    return path

