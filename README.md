# tonixxx: a virtual distillery for cross-compiling binaries

![tonixxx-logo](https://raw.githubusercontent.com/mcandre/tonixxx/master/tonixxx.png)

# EXAMPLE

```console
$ cd examples/fewer

$ tonixxx boil
2018/03/11 12:53:50 All builds completed successfully. Select artifacts may appear in ~/.tonixxx/fewer/bin

$ tree ~/.tonixxx/fewer/bin
~/.tonixxx/fewer/bin
├── dragonflybsd-amd64
│   └── fewer
├── freebsd-amd64
│   └── fewer
├── hardenedbsd-amd64
│   └── fewer
├── illumos-amd64
│   └── fewer
├── linux-gnu-amd64
│   └── fewer
├── linux-musl-amd64
│   └── fewer
├── macos-amd64
│   └── fewer
├── minix-i386
│   └── fewer
├── openbsd-amd64
│   └── fewer
└── windows-amd64
    └── fewer.exe
```

# ABOUT

tonixxx assists developers in cross-compiling software projects. No need for dedicated hardware, nor dual booting, nor configuring obscure toolchains. Instead, tonixxx pours your code through arrays of Vagrant boxes, yielding robust, reliable binaries targeting assorted operating system kernel environments. tonixxx is language and platform agnostic, so you can build to and from a multitude of operating systems, and build projects written in a multitude of programming languages. It's great for automating software ports from your laptop or CI server!

# DOWNLOAD

https://github.com/mcandre/tonixxx/releases

# API DOCUMENTATION

https://godoc.org/github.com/mcandre/tonixxx

## How to cross-compile your project with tonixxx

1. List your build steps in a `tonixxx.yaml` file.
2. Label your build bots.
3. Run `tonixxx boil`.

See [examples/](https://github.com/mcandre/tonixxx/tree/master/examples) for more details on configuring tonixxx.yaml recipes and provisioning build bots for a variety of programming languages.

See `tonixxx -help` for more information on tonixxx invocation syntax.

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
* [Docker](https://www.docker.com/)
* [Mage](https://magefile.org/) (e.g., `go get github.com/magefile/mage`)
* [goimports](https://godoc.org/golang.org/x/tools/cmd/goimports) (e.g. `go get golang.org/x/tools/cmd/goimports`)
* [golint](https://github.com/golang/lint) (e.g. `go get github.com/golang/lint/golint`)
* [errcheck](https://github.com/kisielk/errcheck) (e.g. `go get github.com/kisielk/errcheck`)
* [nakedret](https://github.com/alexkohler/nakedret) (e.g. `go get github.com/alexkohler/nakedret`)
* [goxcart](https://github.com/mcandre/goxcart) (e.g., `github.com/mcandre/goxcart/...`)
* [zipc](https://github.com/mcandre/zipc) (e.g. `go get github.com/mcandre/zipc/...`)

## INSTALL FROM REMOTE GIT REPOSITORY

```console
$ go get github.com/mcandre/tonixxx/...
```

(Yes, include the ellipsis as well, it's the magic Go syntax for downloading, building, and installing all components of a package, including any libraries and command line tools.)

## INSTALL FROM LOCAL GIT REPOSITORY

```console
$ mkdir -p "$GOPATH/src/github.com/mcandre"
$ git clone https://github.com/mcandre/tonixxx.git "$GOPATH/src/github.com/mcandre/tonixxx"
$ cd "$GOPATH/src/github.com/mcandre/tonixxx"
$ git submodule update --init
$ go install ./...
```

## UNINSTALL

```console
$ mage uninstall
```

## LINT

```console
$ mage lint
```

## PORT

```console
$ mage port
```

# HONORABLE MENTIONS

* Cross-platform [toolchains](https://elinux.org/Toolchains) can be configured, though the process is fairly masochistic.
* [Docker](https://www.docker.com/) is a fantastic resource for projects targeting the Linux kernel (from different host operating systems!)
* [Go](https://golang.org/) provides superb out-of-the-box cross-compilation features. As a matter of fact, tonixxx was inspired as a workaround in order to support cross-compilation for non-Go projects, including Rust, C, and C++.

# LICENSE

FreeBSD

# DISCLAIMER

tonixxx is not magic, but simply a wrapper for virtual machines and other Vagrant hypervisors. To truly port an application of practical value, you will likely need to work on the portions of your app that are platform-specific. This may mean, for example, omitting readline when targeting Windows; or rewriting build scripts from Windows-centric .BAT files in terms of a more cross-platform build system. tonixxx will not write the code for you, but it will help you to more quickly test how your source code behaves with different environments. As you port your app to more platforms, you may realize some subroutines failing to compile, or crashing, or even performing faster compared to builds for other platforms. Good luck and keep coding!
