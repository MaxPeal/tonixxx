# tonixxx: a virtual distillery for cross-compiling binaries

# ABOUT

tonixxx assists developers in managing cross-platform software builds. No need for dedicated hardware for each platform, no need for dual booting. tonixxx pours your code through arrays of Vagrant boxes, yielding robust, reliable binaries for different operating system kernels. It's great for managing software ports from your laptop or CI server!

# EXAMPLES

## C

```console
$ cd examples/fewer

$ tonixxx boil
...
2018/03/11 12:53:50 All builds completed successfully. Select artifacts may appear in /Users/andrew/.tonixxx/fewer/bin

$ tree ~/.tonixxx/fewer/bin
/Users/andrew/.tonixxx/fewer/bin
├── freebsd-amd64
│   └── fewer
└── gnu-linux-amd64
    └── fewer
```

## C++

```console
$ cd examples/palindrome

$ tonixxx boil
...
2018/03/13 14:34:19 All builds completed successfully. Select artifacts may appear in /Users/andrew/.tonixxx/palindrome/bin

$ tree ~/.tonixxx/palindrome/bin
/Users/andrew/.tonixxx/palindrome/bin
├── freebsd-amd64
│   └── palindrome
└── gnu-linux-amd64
    └── palindrome
```

## Haskell

```console
$ cd examples/mo

$ tonixxx boil
...
2018/03/12 16:39:49 All builds completed successfully. Select artifacts may appear in /Users/andrew/.tonixxx/mo/bin

$ tree ~/.tonixxx/mo/bin
/Users/andrew/.tonixxx/mo/bin
├── freebsd-amd64
│   └── mo
└── gnu-linux-amd64
    └── mo
```

See `tonixxx -help` for more information.

# RUNTIME REQUIREMENTS

* [Vagrant](https://www.vagrantup.com/)
* [vagrant-rsync-back](https://github.com/smerrill/vagrant-rsync-back) (`vagrant plugin install vagrant-rsync-back`)
* Relevant hypervisors for your configured Vagrant boxes (e.g. VirtualBox, VMware, qemu, Hyper-V)
* Your choice of Vagrant boxes for targeted compilation, with synced folders enabled using rsync.
* Sufficient available RAM and disk space for your Vagrant boxes

## Optional

* [tree](https://linux.die.net/man/1/tree)

# BUILDTIME REQUIREMENTS

* [Go](https://golang.org/) 1.9+

## Recommended

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
* [Go](https://golang.org/) provides superb out-of-the-box cross-compilation features. As a matter of fact, tonixxx was inspired as a workaround in order to support cross-compilation for non-Go projects, including Rust, C, and C++.
