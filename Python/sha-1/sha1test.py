import hashlib
import unittest
import sha1

class ShaTest(unittest.TestCase):

    def test_similar(self):
        abc = sha1.SHA1("abc")
        acc = sha1.SHA1("acc")
        self.assertNotEqual(abc.get_digest(), acc.get_digest(), "Should be different")

    def test_same(self):
        abc1 = sha1.SHA1("hey now, you're an all star")
        abc2 = sha1.SHA1("hey now, you're an all star")
        self.assertEqual(abc1.get_digest(), abc2.get_digest())

    def test_lib(self):
        abc = sha1.SHA1("green")
        lib = hashlib.sha1("green".encode())
        self.assertEqual(abc.get_digest(), lib.hexdigest())


if __name__ == '__main__':
    unittest.main()
