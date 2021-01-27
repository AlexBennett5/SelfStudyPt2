import itertools
import heapq

REMOVED = 'REMOVED'
counter = itertools.count()

class PriorityQueue():

    def __init__(self):
        self.pq = []
        self.ef = {}

    def push(self, priority, task):
        if task in self.ef:
            self.remove_task(task)
        count = next(counter)
        entry = [priority, count, task]
        self.ef[task] = entry
        heapq.heappush(self.pq, entry)

    def remove_task(self, task):
        entry = self.ef.pop(task)
        entry[-1] = REMOVED

    def pop(self):
        while self.pq:
            priority, count, task = heapq.heappop(self.pq)
            if task is not REMOVED:
                del self.ef[task]
                return priority, task

    def get_len(self):
        return len(self.pq)
