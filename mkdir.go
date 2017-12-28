package tonixxx

import "os"

// EnsureDirectory checks that a directory path is allocated.
func EnsureDirectory(pth string) error {
	if _, err := os.Stat(pth); os.IsNotExist(err) {
		return os.Mkdir(pth, os.ModeDir|0755)
	}

	return nil
}
