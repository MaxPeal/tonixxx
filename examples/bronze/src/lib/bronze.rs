#![deny(warnings)]
#![cfg_attr(feature="clippy", feature(plugin))]
#![cfg_attr(feature="clippy", plugin(clippy))]

use std::f64::consts::PI;

#[test]
fn smoketest() {
  assert_eq!(factor_pi(PI), (PI, 1.0));
}

pub fn factor_pi(p: f64) -> (f64, f64) {
  (PI, p/PI)
}
