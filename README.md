# tonixxx: a virtual distillery for boiling binaries

# EXAMPLE

```console
$ cd examples/fewer
$ tonixxx boil
...
$ tree .tonixxx/artifacts
```

# ABOUT

tonixxx masters cross-platform builds by pouring your code through arrays of Vagrant boxes, yielding robust, reliable binaries for multiple operating system kernels. It's great for managing software ports from your laptop or CI server!

# RUNTIME REQUIREMENTS

* [Vagrant](https://www.vagrantup.com/)
* Relevant hypervisors for your configured Vagrant boxes (e.g. VirtualBox, VMware, Hyper-V)

## Optional

* [tree](https://linux.die.net/man/1/tree)

# BUILDTIME REQUIREMENTS

* [Go](https://golang.org/) 1.9+

# HONORABLE MENTIONS

* Cross-platform [toolchains](https://elinux.org/Toolchains) can be configured, though the process is fairly masochistic.
* [Docker](https://www.docker.com/) is a fantastic resource for projects targeting the Linux kernel from different operating systems.
* [gox](https://github.com/mitchellh/gox) loops over all available Go targets, helping developers to quickly produce ports for a wide variety of different target environments. gox inspired tonixxx to support porting applications for Rust and other systems languages!
