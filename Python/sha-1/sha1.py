
class SHA1:

    def __init__(self, msg):
        self.data = bytes(msg, 'utf-8')
        self.msg = ''.join(format(x, 'b').zfill(8) for x in self.data) 
        self.h = [ 0x67452301,
                   0xEFCDAB89,
                   0x98BADCFE,
                   0x10325476,
                   0xC3D2E1F0 ]
        self.preprocess()
        self.msg_to_chunks()
        self.hash()
        self.digest()

    def preprocess(self):
        length = len(self.msg)
        self.msg += "1"
        fill_size = (len(self.msg) // 512) + 1
        fill_size = (fill_size * 512) - 64

        if len(self.msg) % 512 != 448:
            self.msg = self.msg.ljust(fill_size, '0')

        bit_len = bin(length)[2:]
        bit_len = bit_len.zfill(64)
        self.msg += bit_len

    def msg_to_chunks(self):
        self.chunks = [self.msg[i : i+512] for i in range(0, len(self.msg), 512)]

    def left_rotate(self, a, b):
        return ((a << b) | (a >> (32 - b))) & 0xFFFFFFFF

    def chunk_to_words(self, chunk):
        words = [chunk[i : i+32] for i in range(0, 512, 32)]
        more_words = [0] * 80

        for i in range(0, len(words)):
            more_words[i] = int(words[i], 2)
        for i in range(16, 80):
            next_val = self.left_rotate(more_words[i - 3] ^ more_words[i - 8] ^ more_words[i - 14] ^ more_words[i - 16], 1)
            more_words[i] = next_val
        return more_words

    def hash(self):
        for chunk in self.chunks:
            words = self.chunk_to_words(chunk)
            a, b, c, d, e = self.h

            for i in range(0, len(words)):
                if 0 <= i <= 19:
                    f = d ^ (b & (c ^ d))
                    k = 0x5A827999
                elif 20 <= i <= 39:
                    f = b ^ c ^ d
                    k = 0x6ED9EBA1
                elif 40 <= i <= 59:
                    f = (b & c) | (b & d) | (c & d)
                    k = 0x8F1BBCDC
                elif 60 <= i <= 79:
                    f = b ^ c ^ d
                    k = 0xCA62C1D6
                
                temp = (self.left_rotate(a, 5) + f + e + k + words[i]) & 0xFFFFFFFF
                e = d
                d = c
                c = self.left_rotate(b, 30)
                b = a
                a = temp

            self.h = [ self.h[0] + a & 0xFFFFFFFF,
                       self.h[1] + b & 0xFFFFFFFF,
                       self.h[2] + c & 0xFFFFFFFF,
                       self.h[3] + d & 0xFFFFFFFF,
                       self.h[4] + e & 0xFFFFFFFF ]

    def digest(self):
        self.digest = hex(self.h[0])[2:].zfill(8) + hex(self.h[1])[2:].zfill(8) + hex(self.h[2])[2:].zfill(8) + hex(self.h[3])[2:].zfill(8) + hex(self.h[4])[2:].zfill(8)

    def get_digest(self):
        return self.digest


def loop():
    data = input("Enter expression to be hashed: ")
    crypto = SHA1(data)
    print(crypto.get_digest())

if __name__ == '__main__':
    while True:
        loop()
