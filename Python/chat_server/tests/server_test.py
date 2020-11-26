import unittest
import chat_server.chat_server.server

class ServerTestCase(unittest.TestCase):
    def setUp(self):
        self.server = Server("", 1234, 10)

    


if __name__ == '__main__':
    unittest.main()
