# fewer: because `less` is `more`

The fewer command line tool navigates files one byte at a time, printing the `n`ext byte and the `n`ext as a hexadecimal pair, occasionally `r`eading hexadecimal pairs and printing the corresponding ASCII character, until you `q`uit. It's not particularly useful.

# EXAMPLE

```console
$ fewer README.md
> n
23
> n
20
> n
66
> r
23
#
> q

$
```

# BUILD

```console
$ cmake . && make
```
