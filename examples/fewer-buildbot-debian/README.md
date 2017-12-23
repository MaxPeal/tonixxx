# BUILD AND IMPORT VAGRANT BOX

```console
$ vagrant up
$ vagrant package --output fewer-buildbot-debian.box
$ vagrant add --force --name mcandre/fewer-buildbot-debian fewer-buildbot-debian.box
```
