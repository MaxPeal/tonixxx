package tonixxx

import (
	"errors"
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

// ConfigFile names the host path to the tonixxx default configuration file.
func ConfigFile() (string, error) {
	dir, err := os.Getwd()

	if err != nil {
		return "", err
	}

	return path.Join(dir, TonixxxConfigBasename), nil
}

// Parse transforms a byte sequence into a Distillery struct.
func (o *Distillery) Parse(data []byte) error {
	return yaml.UnmarshalStrict(data, o)
}

// Load reads and parses a Distillery struct from a YAML file on disk.
func (o *Distillery) Load(pth string) error {
	contentYAML, err := ioutil.ReadFile(pth)

	if err != nil {
		return err
	}

	if err := o.Parse(contentYAML); err != nil {
		return err
	}

	if err := o.Validate(); err != nil {
		return err
	}

	for _, recipe := range o.Recipes {
		if err := recipe.Validate(); err != nil {
			return err
		}
	}

	return nil
}

// Validate applies some semantic checks to a Distillery configuration.
func (o Distillery) Validate() error {
	if len(o.Recipes) < 1 {
		return errors.New("Distillery configuration missing at least one recipe.")
	}

	return nil
}

// CheckData checks the TonixxxData host directory.
func (o Distillery) CheckData() error {
	if _, err := os.Stat(TonixxxData); os.IsNotExist(err) {
		return os.Mkdir(TonixxxData, os.ModeDir|0755)
	}

	return nil
}

// Up ensures that the configured Vagrant boxes are booted.
func (o Distillery) Up() error {
	if err := o.CheckData(); err != nil {
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
	if err := o.CheckData(); err != nil {
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
	if err := o.CheckData(); err != nil {
		return err
	}

	for _, recipe := range o.Recipes {
		if err := recipe.Clean(); err != nil {
			log.Print(err)
		}
	}

	return o.RemoveData()
}
