import unittest
import dijkstra

class DijkstraTest(unittest.TestCase):

    def test_invalid_start(self):
        graph = Graph([("a", "b", 5)])
        self.assertRaises(RuntimeException, graph.dijkstra("c", "b"))

    def test_negative_edge(self):
        graph = Graph([("a", "b", 5), ("b", "c", -4)])
        self.assertRaises(RuntimeException, graph.dijkstra("a", "c"))

    def test_single_edge(self):
        graph = Graph([("a", "b", 5)])
        path, cost = graph.dijkstra("a", "b")
        self.assertEqual(path, ['a', 'b'])
        self.assertEqual(cost, 5)

    def test_two_edges(self):
        graph = Graph([("a", "b", 5), ("b", "c", 3)])
        path, cost = graph.dijkstra("a", "c")
        self.assertEqual(path, ['a', 'b', 'c'])
        self.assertEqual(cost, 8)

    def test_two_possible_paths(self):
        graph = Graph([("a", "b", 5), ("b", "c", 3), ("a", "c", 1)])
        path, cost = graph.dijkstra("a", "c")
        self.assertEqual(path, ['a', 'c'])
        self.assertEqual(cost, 1)

    def test_dead_end_path(self):
        graph = Graph([("a", "b", 5), ("b", "c", 3), ("c", "d", 3), ("a", "e", 3)])
        path, cost = graph.dijkstra("a", "e")
        self.assertEqual(path, ['a', 'e'])
        self.assertEqual(cost, 3)

    def test_global_optimal_local_suboptimal(self):
        graph = Graph([("a", "b", 5), ("b", "c", 3), ("c", "d", 3), ("a", "e", 7), ("e", "d", 2)])
        path, cost = graph.dijkstra("a", "d")
        self.assertEqual(path, ['a', 'e', 'd'])
        self.assertEqual(cost, 9)

    def test_pentagram(self):
        graph = Graph([("a", "b", 7), ("a", "c", 9), ("a", "f", 14), ("b", "c", 10), ("b", "d", 15), ("c", "d", 11), ("c", "f", 2), ("f", "e", 9), ("d", "e", 6)])
        path, cost = graph.dijkstra("a", "e")
        self.assertEqual(path, ['a', 'c', 'f', 'e'])
        self.assertEqual(cost, 20)


if __name__ == '__main__':
    unittest.main()
