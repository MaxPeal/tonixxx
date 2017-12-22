# tonixxx ("tonics"): a virtual distillery for cross-platform systems programming

# EXAMPLE

```console
$ cd examples/fewer
$ tonixxx boil
...
```

# ABOUT

tonixxx applies [Vagrant](https://www.vagrantup.com/) virtual machines to the task of cross-compiling and cross-testing projects for different operating systems, saving you time and effort when configuring build bots. tonixxx simplifies the process of setting up build bots for systems language projects like C, C++, D, Rust, Swift, and so on, so you can support more platforms with minimal development effort. And if you are using a language with native cross-platform support like Go or Java, you can still benefit from tonixxx helping you test how your code actually behaves in different environments!

The `tonixxx` command line tool loops over each environment target and task described in `tonixxx.yaml`, spinning virtual machines up and down as needed. tonixxx can compile application binaries, assemble dynamic libraries, execute testing frameworks, any command line workflow you prefer. Select artifacts are copied to `.tonixxx/artifacts`, organized by environment name, ready for packaging and publishing.

A default sequence of commands can be specified, with template variables like `${ARTIFACTS}` for conveniently referring to the `.tonixxx/artifacts` directory. For Windows COMSPEC builds, a `${ARTIFACTS_WINDOWS}` template variable offers the same directory as a backslashed Windows path.

Ideally, the exact same commands can be used to build a project for any kernel, but tonixxx recognizes that this is not always the case (e.g. `cp` vs `copy`), so the command list can be overridden per-environment whenever necessary.

As a bonus, overriding commands can also be helpful for auxiliary build tasks that may not produce artifacts, like linting, for easy integration into pass/fail Continuous Integration systems.

In addition to being cross-platform, tonixxx is highly agnostic with respect to software development considerations. In particular, tonixxx operates independently of:

* host and target operating systems (FreeBSD, Linux, macOS, Windows, ...)
* host and target operating system variants (glibc, musl, uClibC, ...)
* host and target architectures (amd64, ARM, i386, SPARC, ...)
* programming languages (C, C++, D, Rust, Swift, ...)
* shell interpreters (ash, bash, cmd.exe, csh, ksh, POSIX sh, PowerShell, ...)
* file systems (APFS, ext2, ext3, ext4, Hammer, HFS+J, NTFS, ...)
* build tools (Ant, automake, cmake, make, Maven, Gradle, Grunt, Gulp, SBT, ...)
* build task workflows (build, checkout, coverage, doc, install, lint, package, test, ...)
* version control systems (CVS, git, Mercurial, Subversion, ...)

tonixxx does the bare minimum to delegate build commands to your bots and fetch the results, stepping out of your way to afford you room to develop your cross-platform project however you need.

# RUNTIME REQUIREMENTS

* [Vagrant](https://www.vagrantup.com/)
* Relevant hypervisors for your configured Vagrant boxes (e.g. VirtualBox, VMware, Hyper-V)

# BUILDTIME REQUIREMENTS

* [Go](https://golang.org/) 1.9+

# HONORABLE MENTIONS

* Cross-platform [toolchains](https://elinux.org/Toolchains) can be configured, though the process may be painful.
* [gox](https://github.com/mitchellh/gox) loops over all available `GOOS`, `GOARCH` values, helping developers to quickly produce ports for a wide variety of different target environments. gox inspired tonixxx!
* [Docker](https://www.docker.com/) is a fantastic resource for projects targeting the Linux kernel, enabling a limited form of cross-compilation: macOS/Windows/Linux -> Linux. Docker also helps to target alt-libc operating systems like musl (Alpine) and uClibC (busybox).
* [fpm](https://github.com/jordansissel/fpm) assists in constructing packages for a variety of operating systems, super cool!
* [YAML](http://yaml.org/) was specifically chosen for its ability to comment out sections, such as for documentation and quick debugging. JSON is quite awful for its lack of comment syntax.
