
pub const DIGEST_LENGTH: usize = 20;

pub struct SHA1 {
  state: SHA1_state,
  buffer: Buffer,
  len: u64,
}

pub struct SHA1_state {
  state: [u32; 5]
}

pub const DEFAULT_STATE: SHA1_state = SHA1_state { state: [0x67452301, 0xefcdab89, 0x98badcfe, 0x10325476, 0xc3d2e1f0] };

struct Buffer {
  len: u32,
  block: [u8; 64],
}

impl Default for SHA1 {
  fn default() -> SHA1 {
    SHA1::new()
  }
}

impl SHA1 {

  pub fn new() -> SHA1 {
    SHA1 {
      state: DEFAULT_STATE,
      len: 0,
      buffer: Buffer {
        len: 0,
        block: [0; 64],
      },
    }
  }

  pub fn from<D: AsRef<[u8]>>(data: D) -> SHA1 {
    let mut rv = SHA1::new();
    rv.update(data.as_ref());
    rv
  }

  pub fn reset(&mut self) {
    self.state = DEFAULT_STATE;
    self.len = 0;
    self.buffer.len = 0;
  }

  pub fn update(&mut self, data: &[u8]) {
    let len = &mut self.len;
    let state = &mut self.state;
    self.buffer.input(data, |buf| {
        *len += buf.len() as u64;
        state.process(buf);
    })
  }

  pub fn digest(&self) -> Digest {
    let mut state = self.state;
    let bits = (self.len + (self.blocks.len as u64)) * 8;
    let extra = [(bits >> 56) as u8,
                 (bits >> 48) as u8,
                 (bits >> 40) as u8,
                 (bits >> 32) as u8,
                 (bits >> 24) as u8,
                 (bits >> 16) as u8,
                 (bits >> 8) as u8,
                 (bits >> 0) as u8];
    let mut last = [0; 128];
    let blocklen = self.blocks.len as usize;
    last[..blocklen].clone_from_slice(&self.blocks.block[..blocklen]);
    last[blocklen] = 0x80;
  
    if blocklen < 56 {
      last[56..64].clone_from_slice(&extra);
      state.process(as_block(&last[0..64]));
    } else {
      last[120..128].clone_from_slice(&extra);
      state.process(as_block(&last[0..64]));
      state.process(as_block(&last[64..128]));
    }

    Digest { data: state }
  }

  pub fn hexdigest(&self) -> std::string::String {
    use std::string::ToString;
    self.digest().to_string()
  }
}

impl Digest {

  pub fn bytes(&self) -> [u8; DIGEST_LENGTH] {
    [(self.data.state[0] >> 24) as u8,
     (self.data.state[0] >> 16) as u8,
     (self.data.state[0] >> 8) as u8,
     (self.data.state[0] >> 0) as u8,
     (self.data.state[1] >> 24) as u8,
     (self.data.state[1] >> 16) as u8,
     (self.data.state[1] >> 8) as u8,
     (self.data.state[1] >> 0) as u8,
     (self.data.state[2] >> 24) as u8,
     (self.data.state[2] >> 16) as u8,
     (self.data.state[2] >> 8) as u8,
     (self.data.state[2] >> 0) as u8,
     (self.data.state[3] >> 24) as u8,
     (self.data.state[3] >> 16) as u8,
     (self.data.state[3] >> 8) as u8,
     (self.data.state[3] >> 0) as u8,
     (self.data.state[4] >> 24) as u8,
     (self.data.state[4] >> 16) as u8,
     (self.data.state[4] >> 8) as u8,
     (self.data.state[4] >> 0) as u8]
  }
}

impl Buffer {

    fn input<F>(&mut self, mut input: &[u8], mut f: F)
        where F: FnMut(&[u8; 64]) {
      
      if self.len > 0 {
        let len = self.len as usize;
        let amt = cmp::min(input.len(), self.block.len() - len);
        self.block[len..len+amt].clone_from_slice(&input[..amt]);
        
        if len + amt == self.block.len() {
          f(&self.block);
          self.len = 0;
          input = &input[amt..];
        } else {
          self.len += amt as u32;
          return;
        }
      }

      assert_eq!(self.len, 0);

      for chunk in input.chunks(64) {
        if chunk.len() == 64 {
          f(as_block(chunk))
        } else {
          self.block[..chunk.len()].clone_from_slice(chunk);
          self.len = chunk.len() as u32;
        }
      }
    }
}

const K0: u32 = 0x5A827999u32;
const K1: u32 = 0x6ED9EBA1u32;
const K2: u32 = 0x8F1BBCDCu32;
const K3: u32 = 0xCA62C1D6u32;

impl SHA1_State {
  
  fn process(&mut self, block: &[u8; 64]) {
    let mut words = [0u32; 16];
    for i in 0..16 {
      let off = i * 4;
      words[i] = 
    }
  }
}
