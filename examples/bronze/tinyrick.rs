//! Build configuration

extern crate tinyrick;
extern crate tinyrick_extras;

use std::fs;
use std::path;

/// Final binary output directory
static BIN : &str = "bin";

/// Generate API documentation
fn doc() {
  tinyrick_extras::doc();
}

/// Run clippy
fn clippy() {
  tinyrick_extras::clippy();
}

/// Validate documentation and run linters
fn lint() {
  tinyrick::deps(doc);
  tinyrick::deps(clippy);
}

/// Install binaries
fn install() {
  tinyrick_extras::install_binaries();
}

/// Uninstall binaries
fn uninstall() {
  tinyrick_extras::uninstall_binaries();
}

/// Doc, lint, and run tests
fn test() {
  tinyrick::deps(lint);
  tinyrick_extras::unit_test();
}

/// Doc, lint, test and compile project
fn build() {
  tinyrick::deps(test);
  tinyrick_extras::build();

  //
  // Group binary artifacts into a single directory for ease of porting
  //

  fs::create_dir_all(BIN).unwrap();

  let release_pathbuf : path::PathBuf = path::Path::new("target")
    .join("release");

  let bin_pathbuf : &path::Path = path::Path::new(BIN);

  for child in &["bronze"] {
    let child_suffixed = format!("{}{}", child, tinyrick::binary_suffix());

    fs::copy(release_pathbuf.join(&child_suffixed), bin_pathbuf.join(&child_suffixed)).unwrap();
  }
}

/// Publish to crate repository
fn publish() {
  tinyrick_extras::publish();
}

/// Delete binary directory
fn clean_bin() {
  if path::Path::new(BIN).exists() {
    fs::remove_dir_all(BIN).unwrap();
  }
}

/// Clean workspaces
fn clean() {
  tinyrick::deps(clean_bin);
  tinyrick_extras::clean_cargo();
}

/// CLI entrypoint
fn main() {
  tinyrick::phony!(clean_bin, clean);

  tinyrick::wubba_lubba_dub_dub!(
    build;
    doc,
    clippy,
    lint,
    test,
    install,
    uninstall,
    publish,
    clean_bin,
    clean
  );
}
