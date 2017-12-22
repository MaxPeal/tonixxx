package tonixxx

import (
	"io/ioutil"
	"log"
	"os"
	"path"

	"gopkg.in/yaml.v2"
)

// Distillery describes a multi-platform build configuration.
type Distillery struct {
	Steps   []string
	Recipes []Recipe
}

// EnsureData checks the TonixxxData host directory is allocated.
func (o Distillery) EnsureData() error {
	if _, err := os.Stat(TonixxxData); os.IsNotExist(err) {
		return os.Mkdir(TonixxxData, os.ModeDir)
	}

	return nil
}

// Up ensures that the configured Vagrant boxes are booted.
func (o Distillery) Up() error {
	if err := o.EnsureData(); err != nil {
		return err
	}

	for _, recipe := range o.Recipes {
		if err := recipe.SpinUp(); err != nil {
			return err
		}
	}

	return nil
}

// Boil executes configured recipes for a distillery.
func (o Distillery) Boil() error {
	if err := o.Up(); err != nil {
		return err
	}

	for _, recipe := range o.Recipes {
		// Default to top-level steps
		if len(recipe.Steps) == 0 {
			recipe.Steps = o.Steps
		}

		if err := recipe.Boil(); err != nil {
			return err
		}
	}

	return nil
}

// Down pauses the configured Vagrant boxes.
func (o Distillery) Down() error {
	if err := o.EnsureData(); err != nil {
		return err
	}

	for _, recipe := range o.Recipes {
		if err := recipe.SpinDown(); err != nil {
			return err
		}
	}

	return nil
}

// RemoveData removes the TonixxxData host directory.
func (o Distillery) RemoveData() error {
	return os.RemoveAll(TonixxxData)
}

// Clean halts Vagrant boxes and removes the TonixxxData host directory.
func (o Distillery) Clean() error {
	for _, recipe := range o.Recipes {
		if err := recipe.Clean(); err != nil {
			log.Print(err)
		}
	}

	return o.RemoveData()
}

// LoadDistillery reads and parses a Distillery struct from a YAML file on disk.
func LoadDistillery(pth string) (*Distillery, error) {
	contentYAML, err := ioutil.ReadFile(pth)

	if err != nil {
		return nil, err
	}

	distillery := Distillery{}

	err = yaml.Unmarshal(contentYAML, &distillery)

	return &distillery, err
}

// ConfigFile names the host path to the tonixxx default configuration file.
func ConfigFile() (string, error) {
	dir, err := os.Getwd()

	if err != nil {
		return "", err
	}

	return path.Join(dir, TonixxxConfigBasename), nil
}
