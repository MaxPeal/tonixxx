# fewer: because `less` is `more`

The fewer command line tool navigates files one byte at a time, printing the next byte and the next as a hexadecimal pair. It's not particularly useful.

# EXAMPLES

```console
$ fewer
> l README.md
> n
23
> n
20
> n
66
> r 23
#
> q
$
```

See the `h` command for more information.

# CONFIGURATION

## Conventional libc

See `fewer -h` for usage information.

## CloudABI

Supply CloudABI YAML content to stdin. See [cloudabi.yml](cloudabi.yml) and related examples.

A `root` path for restricting file access is required, unless `test` is enabled.

Note that some assertions may trigger an abort when `stderr` is unconfigured.

# LICENSE

FreeBSD

# BUILD

```console
$ git submodule update --init
$ cmake .
$ cmake --build . --config Release
```

# PORT

```console
$ tonixxx boil
```

# PROVISIONING

See `buildbot-src` for more information about buildbot provisioning.
