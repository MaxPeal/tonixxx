package tonixxx

import "os"

func EnsureDirectory(pth string) error {
	if _, err := os.Stat(pth); os.IsNotExist(err) {
		return os.Mkdir(pth, os.ModeDir|0755)
	}
}
