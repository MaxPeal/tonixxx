//! Pi factoring

#![deny(warnings)]
#![cfg_attr(feature="clippy", feature(plugin))]
#![cfg_attr(feature="clippy", plugin(clippy))]

extern crate bronze;

use std::io;

/// CLI entrypoint
fn main() {
  loop {
    let mut line = String::new();

    match io::stdin().read_line(&mut line) {
      Err(err) => {
        eprintln!("{:?}", err)
      }
      Ok(_) => {
        match line.trim().parse::<f64>().map(bronze::factor_pi) {
          Err(err) => {
            eprintln!("{:?}", err)
          }
          Ok(factors) => {
            println!("{:?}", factors)
          }
        }
      }
    }
  }
}
