# palindrome: ASANTAATNASA

A `palindrome` is a sequence that is equivalent to its reverse order, like `LOL`, `q`, or `gungagnug`. The `palindrome` application listens for text input and reports whether the input is a palindrome. `palindrome` also includes a `-t` self-test flag as a quick smoketest for errors.

Caveats:

* Case sensitive
* Punctuation sensitive
* Whitespace sensitive
* High likelihood of error for non-ASCII text

# EXAMPLES

```console
$ palindrome
ana
Palindrome: 1
apple
Palindrome: 0
racecar
Palindrome: 1

$ palindrome -t
$ echo "$?"
0
```

# LICENSE

FreeBSD

# BUILD

```console
$ cmake[3] .
$ cmake[3] --build . --target lint
$ cmake[3] --build . --config Release
$ cmake[3] --build . --target leaks
$ CTEST_OUTPUT_ON_FAILURE=1 ctest[3] -C Release
```

# PORT

```console
$ tonixxx boil
```

# PROVISIONING

See `buildbot-src` for more information about buildbot provisioning.
