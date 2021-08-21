package popcopy

import (
	"io"
	"io/ioutil"
	"log"
	"os"
	"path/filepath"
	"regexp"
)

// Version is semver.
const Version = "0.0.1"

// ImplicitExclusions configures automatic file pattern exclusions.
var ImplicitExclusions = []*regexp.Regexp{
	regexp.MustCompile("\\.git"),
}

// copy recursively copies source files
// to the target directory root path,
// excluding any specified path patterns as well as ImplicitExclusions.
func copy(source string, targetRoot string, exclusions []*regexp.Regexp) error {
	sourceAbs, err := filepath.Abs(source)

	if err != nil {
		return err
	}

	exclusions = append(exclusions, ImplicitExclusions...)

	for _, exclusion := range exclusions {
		if exclusion.MatchString(sourceAbs) {
			return nil
		}
	}

	sourceFI, err := os.Stat(sourceAbs)

	if err != nil {
		return err
	}

	targetRootAbs, err := filepath.Abs(targetRoot)

	if err != nil {
		return err
	}

	sourceBase := filepath.Base(source)
	targetRootAbsWithSourceBase := filepath.Join(targetRootAbs, sourceBase)

	sourceMode := sourceFI.Mode()

	switch {
	case sourceMode.IsDir():
		if err := os.MkdirAll(targetRootAbsWithSourceBase, os.ModeDir|0775); err != nil {
			return err
		}

		childrenFI, err := ioutil.ReadDir(sourceAbs)

		if err != nil {
			return err
		}

		for _, childFI := range childrenFI {
			child := childFI.Name()

			if err := copy(filepath.Join(sourceAbs, child), targetRootAbsWithSourceBase, exclusions); err != nil {
				return err
			}
		}
	default:
		fIn, err := os.Open(sourceAbs)
		defer func() {
			if err = fIn.Close(); err != nil {
				log.Panic(err)
			}
		}()

		if err != nil {
			return err
		}

		fOut, err := os.Create(targetRootAbsWithSourceBase)
		defer func() {
			if err = fOut.Close(); err != nil {
				log.Panic(err)
			}
		}()

		if err != nil {
			return err
		}

		if _, err := io.Copy(fOut, fIn); err != nil {
			return err
		}

		return os.Chmod(targetRootAbsWithSourceBase, sourceMode)
	}

	return nil
}

// Copy recursively copies child file(s) of the source path
// to the target directory root path,
// excluding any specified path patterns as well as ImplicitExclusions.
func Copy(source string, targetRoot string, exclusions []*regexp.Regexp) error {
	sourceAbs, err := filepath.Abs(source)

	if err != nil {
		return err
	}

	for _, exclusion := range exclusions {
		if exclusion.MatchString(sourceAbs) {
			return nil
		}
	}

	targetRootAbs, err := filepath.Abs(targetRoot)

	if err != nil {
		return err
	}

	if err = os.MkdirAll(targetRootAbs, os.ModeDir|0775); err != nil {
		return err
	}

	sourceFI, err := os.Stat(sourceAbs)

	if err != nil {
		return err
	}

	sourceMode := sourceFI.Mode()

	switch {
	case sourceMode.IsDir():
		childrenFI, err := ioutil.ReadDir(sourceAbs)

		if err != nil {
			return err
		}

		for _, childFI := range childrenFI {
			child := childFI.Name()
			if err := copy(filepath.Join(sourceAbs, child), targetRootAbs, exclusions); err != nil {
				return err
			}
		}

		return nil
	default:
		return copy(sourceAbs, targetRootAbs, []*regexp.Regexp{})
	}
}
