import threading
import random
import time


class Philosopher(threading.Thread):

    running = True

    def __init__(self, name, leftfork, rightfork):
        threading.Thread.__init__(self)
        self.name = name
        self.leftfork = leftfork
        self.rightfork = rightfork

    def run(self):
        while (self.running):
            time.sleep(random.uniform(1, 10))
            print("{} is hungry from thinking so much!".format(self.name))
            self.dine()

    def dine(self):
        fork1, fork2 = self.leftfork, self.rightfork

        while self.running:
            fork1.acquire(True)
            status = fork2.acquire(False)
            if status:
                break
            fork1.release()

        self.dinner()
        fork1.release()
        fork2.release()

    def dinner(self):
        print("{} has dinner".format(self.name))
        time.sleep(random.uniform(1, 10))
        print("{} is done with dinner, leaves to think".format(self.name))


def dinner_table():
    forks = [threading.Lock() for n in range(6)]
    philosopher_names = ('Plato', 'Kant', 'Hegel', 'Zizek', 'Confucius', 'Spinoza')
    philosophers = [Philosopher(philosopher_names[i], forks[i % 6], forks[(i+1) % 6])
                    for i in range(6)]

    for p in philosophers:
        p.start()


if __name__ == '__main__':
    dinner_table()
