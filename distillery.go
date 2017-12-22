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
	Steps []Step
	Recipes []Recipe
}

// AddData ensures the TONIXXX_DATA host directory is allocated.
func (o Distillery) EnsureData() error {
	if _, err := os.Stat(TONIXXX_DATA); os.IsNotExist(err) {
		return os.Mkdir(TONIXXX_DATA, os.ModeDir)
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

// RemoveData removes the TONIXXX_DATA host directory.
func (o Distillery) RemoveData() error {
	return os.RemoveAll(TONIXXX_DATA)
}

// Clean halts Vagrant boxes and removes the TONIXXX_DATA host directory.
func (o Distillery) Clean() error {
	for _, recipe := range(o.Recipes) {
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

func ConfigFile() (string, error) {
	dir, err := os.Getwd()

	if err != nil {
		return "", err
	}

	return path.Join(dir, TONIXXX_CONFIG_BASENAME), nil
}
