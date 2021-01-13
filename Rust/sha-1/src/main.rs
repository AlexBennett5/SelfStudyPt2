mod sha1;

fn main() {
  let mut crypto = sha1::SHA1::new();
  crypto.update(b"abc");
  println!("{}", hex::encode(crypto.digest()));
}
