import unittest
import dijkstra

class DijkstraTest(unittest.TestCase):

    def test_invalid_start(self):
        graph = {'a': {'b': 5},
                 'b': {}}
        self.assertRaises(RuntimeError, lambda: dijkstra.dijkstra(graph, 'c', 'b'))

    def test_negative_edge(self):
        graph = {'a': {'b': 5},
                 'b': {'c': -4},
                 'c': {}}
        self.assertRaises(RuntimeError, lambda: dijkstra.dijkstra(graph, 'a', 'c'))

    def test_single_edge(self):
        graph = {'a': {'b': 5},
                 'b': {}}
        path, cost = dijkstra.dijkstra(graph, 'a', 'b')
        self.assertEqual(path, ['a', 'b'])
        self.assertEqual(cost, 5)

    def test_cycle(self):
        graph = {'a': {'b': 2},
                 'b': {'a': 2}}
        path, cost = dijkstra.dijkstra(graph, 'a', 'b')
        self.assertEqual(path, ['a', 'b'])
        self.assertEqual(cost, 2)

    def test_two_edges(self):
        graph = {'a': {'b': 5},
                 'b': {'c': 3},
                 'c': {}}
        path, cost = dijkstra.dijkstra(graph, 'a', 'c')
        self.assertEqual(path, ['a', 'b', 'c'])
        self.assertEqual(cost, 8)

    def test_two_possible_paths(self):
        graph = {'a': {'b': 5, 'c': 1},
                 'b': {'c': 3},
                 'c': {}}
        path, cost = dijkstra.dijkstra(graph, 'a', 'c')
        self.assertEqual(path, ['a', 'c'])
        self.assertEqual(cost, 1)

    def test_dead_end_path(self):
        graph = {'a': {'b':5, 'e':3},
                 'b': {'c':3},
                 'c': {'d':3},
                 'd': {},
                 'e': {}}
        path, cost = dijkstra.dijkstra(graph, 'a', 'e')
        self.assertEqual(path, ['a', 'e'])
        self.assertEqual(cost, 3)

    def test_local_suboptimal_global_suboptimal(self):
        graph = {'a': {'b':5, 'e':7},
                 'b': {'c':3},
                 'c': {'d':3},
                 'd': {},
                 'e': {'d':2}}
        path, cost = dijkstra.dijkstra(graph, 'a', 'd')
        self.assertEqual(path, ['a', 'e', 'd'])
        self.assertEqual(cost, 9)

    def test_pentagram(self):
        graph = {'a': {'b':7, 'c':9, 'f':14},
                 'b': {'c':10, 'd':15},
                 'c': {'d':11, 'f':2},
                 'd': {'e':6},
                 'e': {},
                 'f': {'e':9}}
        path, cost = dijkstra.dijkstra(graph, 'a', 'e')
        self.assertEqual(path, ['a', 'c', 'f', 'e'])
        self.assertEqual(cost, 20)


if __name__ == '__main__':
    unittest.main()
