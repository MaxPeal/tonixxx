# popcopy - can i help you?

# ABOUT

popcopy recursively copies child file(s) to the desired destination path, excluding patterns you don't want copied.

# EXAMPLE

```go
import (
	"regexp"
	"github.com/mcandre/popcopy"
)

func main() {
	if err := popcopy.Copy(
		"business-presentations",
		"/media/usbstick",
		[]regexp.Regexp{regexp.MustCompile("Thumbs.db")},
	); err != nil {
		panic(err)
	}
}
```

# DOCUMENTATION

https://godoc.org/github.com/mcandre/popcopy

# CONTRIBUTING

For more information on developing popcopy itself, see [DEVELOPMENT.md](DEVELOPMENT.md).
