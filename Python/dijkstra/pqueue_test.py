import unittest
import pqueue

class PriorityQueueTest(unittest.TestCase):

    def test_add_task(self):
        pq = pqueue.PriorityQueue()
        pq.push(1, 'a')
        self.assertEqual(pq.get_len(), 1)

    def test_heapify(self):
        pq = pqueue.PriorityQueue()
        pq.push(3, 'a')
        pq.push(2, 'b')
        priority, task = pq.pop()
        self.assertEqual(priority, 2)
        self.assertEqual(task, 'b')
        self.assertEqual(pq.get_len(), 1)

    def test_update(self):
        pq = pqueue.PriorityQueue()
        pq.push(3, 'a')
        pq.push(2, 'b')
        pq.push(1, 'a')
        priority, task = pq.pop()
        self.assertEqual(priority, 1)
        self.assertEqual(task, 'a')
        self.assertEqual(pq.get_len(), 2)

if __name__ == '__main__':
    unittest.main()
