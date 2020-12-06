# palindrome: ASANTAATNASA

A `palindrome` is a sequence that is equivalent to its reverse order, like `LOL`, `q`, or `gungagnug`. The `palindrome` application listens for text input and reports whether the input is a palindrome. `palindrome` also includes a `-t` self-test flag as a quick smoketest for errors.

Caveats:

* Case sensitive
* Punctuation sensitive
* Whitespace sensitive
* High likelihood of error for non-ASCII text

# EXAMPLE

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

# DOCUMENTATION

https://mcandre.github.io/tonixxx/examples/palindrome/

# LICENSE

FreeBSD

# BUILD

```console
$ tonixxx boil
```

# CLEAN

```console
$ tonixxx clean
```

# PROVISIONING

See `buildbot-src` for more information about buildbot provisioning.
