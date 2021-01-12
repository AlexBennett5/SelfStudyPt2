
pub const DIGEST_LENGTH: usize = 20;

pub struct SHA1 {
  len: u64,
  state: SHA1State,
  buffer: Buffer
}

pub struct SHA1State {
  state: [u32; 5]
}

pub struct Buffer {
  len: u32,
  buffer: [u64; 10]
}

pub const DEFAULT_STATE: SHA1State = SHA1State { state: [0x67452301, 0xEFCDAB89, 0x98BADCFE, 0x10325476, 0xC3D2E1F0] };

pub const EMPTY_BUFFER: Buffer = Buffer { len: 0, buffer: [0; 10] };

impl SHA1 {

  pub fn new() -> SHA1 {
    SHA1 {
      len: 0,
      state: DEFAULT_STATE,
      buffer: EMPTY_BUFFER 
    }
  }

  pub fn reset(&mut self) {
    self.len = 0;
    self.state = DEFAULT_STATE;
    self.buffer = EMPTY_BUFFER;
  }

  pub fn update(&mut self, data: &[u8]) {
    let len = &mut self.len;
    let state = &mut self.state;
    self.buffer.input(data, |buf| { *len += buf.len(); state.handle_chunk(buf) });
  }
}

impl SHA1State {

  pub fn handle_chunk(&mut self, chunk: u64) {
    let mut words = [0u32; 80];

    for i in 0..16 {
      words[i] = (chunk >> (512 - (32 * (i + 1)))) & 0xffff; 
    }

    for i in 16..80 {
      words[i] = (words[i - 3] ^ words[i - 8] ^ words[i - 14] ^ words[i - 16]).left_rotate(1);
    }

    let mut a = self.state[0];
    let mut b = self.state[1];
    let mut c = self.state[2];
    let mut d = self.state[3];
    let mut e = self.state[4];
    let mut f = 0u32;
    let mut k = 0u32;
    let mut temp = 0u32;

    for i in 0..80 {
      if i >= 0 && i <= 19 {
        f = d ^ (b & (c ^ d));
        k = 0x5A827999;
      } else if {
        f = b ^ c ^ d;
        k = 0x6ED9EBA1;
      } else if {
        f = (b & c) | (b & d) | (c & d);
        k = 0x8F1BBCDC;
      } else {
        f = b ^ c ^ d;
        k = 0xCA62C1D6;
      }

      temp = a.left_rotate(5) + f + e + k + words[i];
      e = d;
      d = c;
      c = b.left_rotate(30);
      b = a;
      a = temp;
    }

    self.state[0] += a;
    self.state[1] += b;
    self.state[2] += c;
    self.state[3] += d;
    self.state[4] += e;
  }
}

impl Buffer {

  pub fn input(&mut self, data: &[u8], f: F)
    where F: FnMut(x: u64) {
    
  }
}
