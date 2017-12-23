package tonixxx

import (
	"crypto/md5"
	"errors"
	"io/ioutil"
	"log"
	"os"
	"path"

	"gopkg.in/yaml.v2"
)

// ProjectBinariesDirectoryBasename locates project-wide binary aggregation at the end of a build, relative to the tonixxx per-project metadata directory.
const ProjectBinariesDirectoryBasename = "bin"

// ProjectNamePattern constrains names in order to use names as-is for file paths while building a project.
var ProjectNamePattern *regexp.Regexp = regexp.MustCompile("^[a-zA-Z\\.\\-_]+$")

// Distillery describes a multi-platform build configuration.
type Distillery struct {
	Project string
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

// ProjectData calculates the per-project tonixxx data directory based on a hash of the project directory absolute path.
func (o Distillery) ProjectData() (string, error) {
	cwd, err := os.Getwd()

	if err != nil {
		return "", err
	}

	cwdAbsolute, err := filepath.Abs(cwd)

	if err != nil {
		return "", err
	}

	return path.Join(TonixxxHome(), o.Project), nil
}

func (o Distillery) ProjectArtifacts(string, error) {
	projectData, err := o.ProjectData()

	if err != nil {
		return "", err
	}

	return path.Join(projectData, ProjectBinaryDirectoryBasename), nil
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
	if !ProjectNamePattern.MatchString(o.Project) {
		return errors.New(fmt.Sprintf("Distillery must have a non-empty project name matching %s", ProjectNamePattern))
	}

	if len(o.Recipes) < 1 {
		return errors.New("Distillery configuration missing at least one recipe.")
	}

	return nil
}

// CheckData checks the TonixxxData host directory.
func (o Distillery) CheckData() error {
	return EnsureDirectory(TonixxxData)
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

		if err := recipe.MergeArtifacts(o.ProjectArtifacts); err != nil {
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
