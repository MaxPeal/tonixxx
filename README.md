# tonixxx: a virtual distillery for cross-compiling binaries

# EXAMPLE

```console
$ cd examples/fewer
$ tonixxx boil
...
$ tree .tonixxx/artifacts
```

See `tonixxx -help` for more information.

# ABOUT

tonixxx masters cross-platform builds by pouring your code through arrays of Vagrant boxes, yielding robust, reliable binaries for multiple operating system kernels. It's great for managing software ports from your laptop or CI server!

# RUNTIME REQUIREMENTS

* [Vagrant](https://www.vagrantup.com/)
* Relevant hypervisors for your configured Vagrant boxes (e.g. VirtualBox, VMware, Hyper-V)

## Optional

* [tree](https://linux.die.net/man/1/tree)

# BUILDTIME REQUIREMENTS

* [Go](https://golang.org/) 1.9+
* sh (e.g. bash)
* [make](https://www.gnu.org/software/make/)
* [zipc](https://github.com/mcandre/zipc)
* [golint](https://github.com/golang/lint)
* [goimports](https://godoc.org/golang.org/x/tools/cmd/goimports)
* [errcheck](https://github.com/kisielk/errcheck)
* [nakedret](https://github.com/alexkohler/nakedret)
* [opennota/check](https://github.com/opennota/check)
* [megacheck](https://github.com/dominikh/go-tools/tree/master/cmd/megacheck)
* [gox](https://github.com/mitchellh/gox), a tool so cool that it inspired tonixxx!

## CHECKOUT DEPENDENCIES

```console
$ git submodule update --init
```

## LINT

```console
$ make lint
```

## BUILD AND INSTALL

```console
$ make install
```

## PORT

```console
$ make port
```

## UNINSTALL

```console
$ make uninstall
```

# HONORABLE MENTIONS

* Cross-platform [toolchains](https://elinux.org/Toolchains) can be configured, though the process is fairly masochistic.
* [Docker](https://www.docker.com/) is a fantastic resource for projects targeting the Linux kernel from different operating systems.
