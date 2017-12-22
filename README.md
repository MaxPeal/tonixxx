# tonixxx ("tonics"): a virtual distillery for cross-platform systems programming

# EXAMPLE

```console
$ cd examples/fewer
$ tonixxx boil
...
```

# ABOUT

tonixxx applies [Vagrant](https://www.vagrantup.com/) virtual machines to the task of cross-compiling and cross-testing projects for different operating systems, saving you time and effort when configuring build bots.

The `tonixxx` command line tool loops over each environment target and task described in `tonixxx.yaml`, spinning virtual machines up and down as needed. tonixxx can compile application binaries, assemble dynamic libraries, execute testing frameworks, any command line workflow you prefer. Select artifacts are copied to `.tonixxx/artifacts`, organized by environment name, ready for packaging and publishing. A default sequence of commands can be specified, with template variables like `${ARTIFACTS}` for conveniently referring to the `.tonixxx/artifacts` directory. For Windows COMSPEC builds, a `${ARTIFACTS_WINDOWS}` template variable offers the same directory as a backslashed Windows path.

# RUNTIME REQUIREMENTS

* [Vagrant](https://www.vagrantup.com/)
* Appropriate hypervisor providers for your configured Vagrant boxes (e.g. VirtualBox, VMware, Hyper-V)

# BUILDTIME REQUIREMENTS

* a C compiler

# HONORABLE MENTIONS

* Cross-platform [toolchains](https://elinux.org/Toolchains) can be configured, though the process may be painful.
* [Go](https://golang.org/) dramatically improves the cross-platform systems programming experience by baking in no-nonsense cross-compilation into the default toolchain. You can build from macOS/Windows/Linux/FreeBSD/DragonflyBSD/NetBSD/... hosts, producing binaries targeting macOS/Windows/Linux/FreeBSD/DragonflyBSD/NetBSD/..., as well as building from-and-to different architectures like amd64/i386/..., simply by configuring the `GOOS`, `GOARCH` environment variables.
* [gox](https://github.com/mitchellh/gox) loops over all available `GOOS`, `GOARCH` values, helping developers to quickly produce ports for a wide variety of different target environments. gox inspired tonixxx!
* [Docker](https://www.docker.com/) is a fantastic resource for projects targeting the Linux kernel, enabling a limited form of cross-compilation: macOS/Windows/Linux -> Linux. Docker also helps to target alt-libc operating systems like musl (Alpine) and uClibC (busybox).
* [fpm](https://github.com/jordansissel/fpm) assists in constructing packages for a variety of operating systems, super cool!
* [YAML](http://yaml.org/) was specifically chosen for its ability to comment out sections, such as for documentation and quick debugging. JSON is quite awful for its lack of comment syntax.
