package tonixxx

import (
	"errors"
	"fmt"
	"io/ioutil"
	"log"
	"os"
	"path"
	"regexp"
	"strconv"

	"github.com/oleiade/lane"
	"gopkg.in/yaml.v2"
)

// ProjectNamePattern constrains names in order to use names as-is for file paths while building a project.
var ProjectNamePattern = regexp.MustCompile(`^[a-zA-Z0-9\.\-_]+$`)

// ConfigFile names the host path to the tonixxx default configuration file.
func ConfigFile() (string, error) {
	dir, err := os.Getwd()

	if err != nil {
		return "", err
	}

	return path.Join(dir, ConfigBasename), nil
}

// Distillery describes a multi-platform build configuration.
type Distillery struct {
	// Project distinguishes this build from other potential projects on the same host.
	//
	// Example: hello
	Project string

	// OutputDirectory optionally specifices where build artifacts are found on guests.
	// If OutputDirectory is nil, then artifacts are not copied back to the host.
	//
	// Example: bin
	// Default: <nil>
	OutputDirectory *string

	// Steps enumerates build steps.
	//
	// Example: []string{"./configure", "make"}
	Steps []string

	// Recipes enumerates build bots.
	//
	// Example: []Recipe{Recipe{Label: "linux", Box: "ubuntu/xenial64"}}
	Recipes []Recipe

	// Debug enables more logging.
	//
	// Example: true
	Debug bool

	// MaxRunningRecipes constrains the number of simultaneously running recipes.
	// Negative indicates no constraint.
	// Zero indicates unset configuration, which becomes DefaultMaxRunningRecipes.
	//
	// Example: 1
	MaxRunningRecipes int

	// runningRecipes tracks running recipes
	runningRecipes *lane.Deque
}

// ProjectData calculates the per-project tonixxx data directory based on a hash of the project directory absolute path.
func (o Distillery) ProjectData() (string, error) {
	tonixxxHome, err := DataHome()

	if err != nil {
		return "", err
	}

	return path.Join(tonixxxHome, o.Project), nil
}

// ProjectArtifacts supplies the host path to the aggregated artifacts produced for a project after any Vagrant builds.
func (o Distillery) ProjectArtifacts() (string, error) {
	projectData, err := o.ProjectData()

	if err != nil {
		return "", err
	}

	return path.Join(projectData, *o.OutputDirectory), nil
}

// Validate applies some semantic checks to a Distillery configuration.
func (o Distillery) Validate() error {
	if !ProjectNamePattern.MatchString(o.Project) {
		return fmt.Errorf("distillery must have a non-empty project name matching %s", ProjectNamePattern)
	}

	if len(o.Recipes) < 1 {
		return errors.New("distillery configuration missing at least one recipe")
	}

	for _, recipe := range o.Recipes {
		if err := recipe.Validate(); err != nil {
			return err
		}
	}

	return nil
}

// UpdateRunningRecipes collects information about Vagrant instances.
func (o Distillery) UpdateRunningRecipes() error {
	for _, recipe := range o.Recipes {
		running, err := recipe.IsRunning()

		if err != nil {
			return err
		}

		if running {
			o.runningRecipes.Append(recipe)
		}
	}

	return nil
}

// Load constructs a Distillery from a YAML file path.
func Load(pth string) (*Distillery, error) {
	distillery := new(Distillery)

	contentYAML, err := ioutil.ReadFile(pth)

	if err != nil {
		return nil, err
	}

	if er := yaml.UnmarshalStrict(contentYAML, distillery); er != nil {
		return nil, er
	}

	maxRecipesString := os.Getenv(MaxRecipesKey)

	//
	// Bleugh. Go's integer parsing API is messy.
	//
	if maxRecipesString != "" {
		maxRecipes, er := strconv.ParseInt(maxRecipesString, 10, 32)

		if er != nil {
			return nil, er
		}

		distillery.MaxRunningRecipes = int(maxRecipes)
	}

	if distillery.MaxRunningRecipes == 0 {
		distillery.MaxRunningRecipes = DefaultMaxRunningRecipes
	}

	projectData, err := distillery.ProjectData()

	if err != nil {
		return nil, err
	}

	var projectArtifacts string

	if distillery.OutputDirectory != nil {
		projectArtifacts, err = distillery.ProjectArtifacts()

		if err != nil {
			return nil, err
		}
	}

	for i := range distillery.Recipes {
		// Inject ProjectData path into recipe
		distillery.Recipes[i].projectData = projectData

		if distillery.OutputDirectory != nil {
			// Inject ProjectArtifacts path into recipe
			distillery.Recipes[i].projectArtifacts = projectArtifacts
		}

		// Default recipe steps to top-level distillery steps
		if len(distillery.Recipes[i].Steps) == 0 {
			distillery.Recipes[i].Steps = distillery.Steps
		}
	}

	if err := distillery.Validate(); err != nil {
		return nil, err
	}

	distillery.runningRecipes = lane.NewDeque()

	if err := distillery.UpdateRunningRecipes(); err != nil {
		return nil, err
	}

	return distillery, nil
}

// Boil executes configured recipes for a distillery.
func (o Distillery) Boil() error {
	for _, recipe := range o.Recipes {
		if o.MaxRunningRecipes > 0 {
			for o.runningRecipes.Size() > o.MaxRunningRecipes-1 {
				if err := o.runningRecipes.Pop().(Recipe).VagrantDown(); err != nil {
					return err
				}
			}

			o.runningRecipes.Append(recipe)
		}

		if err := recipe.Boil(o.OutputDirectory, o.Debug); err != nil {
			return err
		}
	}

	if err := o.Down(); err != nil {
		return nil
	}

	if o.OutputDirectory != nil {
		projectArtifacts, err := o.ProjectArtifacts()

		if err != nil {
			return err
		}

		log.Printf("Artifacts merged to %s", projectArtifacts)
	}

	return nil
}

// Down pauses the configured Vagrant boxes.
func (o Distillery) Down() error {
	for _, recipe := range o.Recipes {
		if err := recipe.VagrantDown(); err != nil {
			return err
		}
	}

	return nil
}

// Clean destroys a project's Vagrant instances boxes and
// removes the project directory from ~/.tonixxx
func (o Distillery) Clean() error {
	for _, recipe := range o.Recipes {
		if err := recipe.Clean(); err != nil {
			log.Print(err)
		}
	}

	projectData, err := o.ProjectData()

	if err != nil {
		return err
	}

	return os.RemoveAll(projectData)
}
