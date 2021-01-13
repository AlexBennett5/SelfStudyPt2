use core::mem;
use std::convert::TryInto;

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
  buffer: [u8; 64]
}

pub const DEFAULT_STATE: SHA1State = SHA1State { state: [0x67452301, 0xEFCDAB89, 0x98BADCFE, 0x10325476, 0xC3D2E1F0] };

pub const EMPTY_BUFFER: Buffer = Buffer { len: 0, buffer: [0u8; 64] };

fn as_chunk(input: &[u8]) -> u64 {
  unsafe {
    assert!(input.len() == 8);
    let arr: [u8; 8] = mem::transmute(input.as_ptr());
    u64::from_be_bytes(arr)
  }
}

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
    self.buffer.input(data, |buf| { *len += 8u64; state.handle_chunk(buf) });
  }

  pub fn finishing_up(&mut self) {
    let mut last = [0u8; 128];
    let mut final_size = 0;
    let buffer_size = self.buffer.len as usize;
    last[..buffer_size].clone_from_slice(&self.buffer.buffer[..buffer_size]);
    last[buffer_size] = 0x80;
    self.buffer = EMPTY_BUFFER;

    if buffer_size > 55 {
      final_size = self.len + 128;
      last[120..128].clone_from_slice(&final_size.to_be_bytes());
      self.update(&last[0..128]);
    } else {
      final_size = self.len + 64;
      last[56..64].clone_from_slice(&final_size.to_be_bytes());
      self.update(&last[0..64]);
    }
  }

  pub fn digest(&mut self) -> [u8; 20] {
    self.finishing_up();
    [(self.state.state[0] >> 24) as u8,
     (self.state.state[0] >> 16) as u8,
     (self.state.state[0] >> 8) as u8,
     (self.state.state[0] >> 0) as u8,
     (self.state.state[1] >> 24) as u8,
     (self.state.state[1] >> 16) as u8,
     (self.state.state[1] >> 8) as u8,
     (self.state.state[1] >> 0) as u8,
     (self.state.state[2] >> 24) as u8,
     (self.state.state[2] >> 16) as u8,
     (self.state.state[2] >> 8) as u8,
     (self.state.state[2] >> 0) as u8,
     (self.state.state[3] >> 24) as u8,
     (self.state.state[3] >> 16) as u8,
     (self.state.state[3] >> 8) as u8,
     (self.state.state[3] >> 0) as u8,
     (self.state.state[4] >> 24) as u8,
     (self.state.state[4] >> 16) as u8,
     (self.state.state[4] >> 8) as u8,
     (self.state.state[4] >> 0) as u8]
  }
}

impl SHA1State {

  pub fn handle_chunk(&mut self, chunk: u64) {
    let mut words = [0u32; 80];

    for i in 0..16 {
      words[i] = ((chunk >> (64 - (4 * (i + 1)))) & 0xffff) as u32; 
    }

    for i in 16..80 {
      words[i] = (words[i - 3] ^ words[i - 8] ^ words[i - 14] ^ words[i - 16]).rotate_left(1);
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
      } else if i >= 20 && i <= 39 {
        f = b ^ c ^ d;
        k = 0x6ED9EBA1;
      } else if i >= 40 && i <= 59 {
        f = (b & c) | (b & d) | (c & d);
        k = 0x8F1BBCDC;
      } else {
        f = b ^ c ^ d;
        k = 0xCA62C1D6;
      }

      temp = a.rotate_left(5) + f + e + k + words[i];
      e = d;
      d = c;
      c = b.rotate_left(30);
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

  pub fn input<F>(&mut self, data: &[u8], mut f: F) 
      where F: FnMut(u64) {
    
    let mut index = 0 as usize;
    let mut remaining = data.len() as usize;

    if self.len > 0 {
      let amt = (64 - self.len) as usize;
      self.buffer[amt..64].clone_from_slice(&data[..amt]);

      for i in (0..64).step_by(8) {
        f(as_chunk(&self.buffer[i..(i + 8)]));
      }

      index += amt;
      remaining -= amt;
    }

    while (remaining / 64) != 0 {
      f(as_chunk(&data[index..(index + 8)]));
      index += 8;
      remaining -= 64;
    } 
    
    if remaining != 0 {
      self.buffer = [0u8; 64];
      self.buffer[..remaining].clone_from_slice(&data[index..]);
      self.len = remaining.try_into().unwrap();
    }
  }
}
