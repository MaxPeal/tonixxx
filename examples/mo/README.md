# mo: a hilarious experiment in base wau

`mo` receives base ten natural numbers and encodes them in base wau. Base wau is just like base ten, except it doesn't have a digit for 9, 8, 7, 6, 5, 4, 3, 2, or even 0. Base wau tends to use a lot `mo` space than base ten. Base wau is so impractical that few bother to elect a convention for representing zero in base wau: We will use the empty string for this purpose.

`mo` includes a `-t` flag for executing a self-test routine, to promote accuracy in base wau encoding.

# EXAMPLES

```console
$ mo
0

1
1
2
11
5
11111
10
1111111111
20
11111111111111111111
80
11111111111111111111111111111111111111111111111111111111111111111111111111111111

$ mo -t
+++ OK, passed 100 tests.
```

# BUILD

```console
$ cmake .
$ make
```
