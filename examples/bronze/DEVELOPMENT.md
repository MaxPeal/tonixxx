# OVERVIEW

bronze's own compilation process is compatible with standard cargo. We wrap some common workflows with tinyrick tasks for convenience, and offer a tonixxx rig to automate building ports.

# BUILDTIME REQUIREMENTS

* [Rust](https://www.rust-lang.org/en-US/) 1.30+

## Recommended

* [clippy](https://github.com/rust-lang-nursery/rust-clippy)
* [tinyrick](https://github.com/mcandre/tinyrick) (e.g., `cargo install tinyrick`)
* [tonixxx](https://github.com/mcandre/tonixxx)

# INSTALL BINARIES FROM LOCAL SOURCE

```console
$ tinyrick install
```

# UNINSTALL BINARIES

```console
$ tinyrick uninstall
```

# BUILD: Doc, Lint, Test, and Compile

```console
$ tinyrick [build]
```

# PORT

```console
$ tonixxx boil
```

# PUBLISH

```console
$ tinyrick publish
```

# CLEAN

```console
$ tinyrick clean
```

# PROVISIONING

See `buildbot-src` for more information about buildbot provisioning.
