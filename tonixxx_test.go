package tonixxx_test

import (
	"testing"

	"github.com/mcandre/tonixxx"
)

func TestVersion(t *testing.T) {
	if tonixxx.Version == "" {
		t.Errorf("Expected tonixxx version to be non-blank")
	}
}
