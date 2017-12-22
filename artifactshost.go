package tonixxx

import (
	"os"
	"path"
)

// ArtifactsHost names the host path where select artifacts may be copied upon building.
func ArtifactsHost() (string, error) {
	dir, err := os.Getwd()

	if err != nil {
		return "", err
	}

	return path.Join(dir, TonixxxData), nil
}
