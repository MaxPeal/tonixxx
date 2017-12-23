package tonixxx

import (
	"bytes"
	"errors"
	"fmt"
	"io/ioutil"
	"log"
	"os"
	"os/exec"
	"path"
	"regexp"

	"github.com/otiai10/copy"
	"gopkg.in/yaml.v2"
)

// ProjectBinariesDirectoryBasename locates project-wide binary aggregation at the end of a build, relative to the tonixxx per-project metadata directory.
const ProjectBinariesDirectoryBasename = "bin"

// ProjectNamePattern constrains names in order to use names as-is for file paths while building a project.
var ProjectNamePattern *regexp.Regexp = regexp.MustCompile("^[a-zA-Z0-9\\.\\-_]+$")

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
	tonixxxHome, err := TonixxxHome()

	if err != nil {
		return "", err
	}

	return path.Join(tonixxxHome, o.Project), nil
}

func (o Distillery) EnsureProjectDirectory() error {
	projectData, err := o.ProjectData()

	if err != nil {
		return err
	}

	return EnsureDirectory(projectData)
}

func (o Distillery) ProjectArtifacts() (string, error) {
	projectData, err := o.ProjectData()

	if err != nil {
		return "", err
	}

	return path.Join(projectData, ProjectBinariesDirectoryBasename), nil
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

// CheckData checks that the TonixxxHome host directory exists.
// If not, CheckData constructs the directory.
func (o Distillery) CheckData() error {
	tonixxxHome, err := TonixxxHome()

	if err != nil {
		return err
	}

	return EnsureDirectory(tonixxxHome)
}

func (o Distillery) CloneHost(recipe Recipe) (string, error) {
	projectData, err := o.ProjectData()

	if err != nil {
		return "", err
	}

	return path.Join(projectData, recipe.Name), nil
}

func (o Distillery) EnsureCloneRecipePath(recipe Recipe) error {
	if err := o.EnsureProjectDirectory(); err != nil {
		return err
	}

	pth, err := o.CloneHost(recipe)

	if err != nil {
		return err
	}

	return EnsureDirectory(pth)
}

// VagrantHostDirectory provides the path to a recipe's Vagrant directory.
func (o Distillery) VagrantHostRecipeDirectory(recipe Recipe) (string, error) {
	projectData, err := o.ProjectData()

	if err != nil {
		return "", nil
	}

	return path.Join(projectData, recipe.Name), nil
}

func (o Distillery) VagrantfilePath(recipe Recipe) (string, error) {
	vagrantHostDir, err := o.VagrantHostRecipeDirectory(recipe)

	if err != nil {
		return "", err
	}

	return path.Join(vagrantHostDir, VagrantfileBasename), nil
}

// Generate a basic Vagrantfile from the recipe base box url.
func (o Distillery) EnsureVagrantfile(recipe Recipe) error {
	contentVagrantfile := fmt.Sprintf("Vagrant.configure('2') do |config|\n  config.vm.box = \"%s\"\nend\n", recipe.Base)

	vagrantfilePath, err := o.VagrantfilePath(recipe)

	if err != nil {
		return err
	}

	contentVagrantfileBytes := []byte(contentVagrantfile)

	return ioutil.WriteFile(vagrantfilePath, contentVagrantfileBytes, 0644)
}

// EnsureClone allocates space for a Vagrant box to be cloned.
func (o Distillery) EnsureCloneRecipe(recipe Recipe) error {
	if err := o.EnsureCloneRecipePath(recipe); err != nil {
		return err
	}

	cwd, err := os.Getwd()

	if err != nil {
		return err
	}

	cloneHost, err := o.CloneHost(recipe)

	if err != nil {
		return err
	}

	// Copy source files into the right Vagrant clone directory.
	if err := copy.Copy(cwd, cloneHost); err != nil {
		return err
	}

	if err := o.EnsureArtifactsRecipeHost(recipe); err != nil {
		return err
	}

	if err := o.EnsureVagrantfile(recipe); err != nil {
		return err
	}

	return nil
}

// ArtifactsHost provides a recipe relative output directory.
// If all builds in a distillery are successful,
// then these artifacts are copied to a project relative directory.
func (o Distillery) ArtifactsRecipeHost(recipe Recipe) (string, error) {
	vagrantHostDir, err := o.VagrantHostRecipeDirectory(recipe)

	if err != nil {
		return "", err
	}

	return path.Join(vagrantHostDir, RecipeBinariesDirectoryBasename), nil
}

func (o Distillery) EnsureArtifactsRecipeHost(recipe Recipe) error {
	artifactsHost, err := o.ArtifactsRecipeHost(recipe)

	if err != nil {
		return err
	}

	return EnsureDirectory(artifactsHost)
}

// VagrantUp boots a Vagrant box.
func (o Distillery) VagrantUp(recipe Recipe) error {
	cloneHost, err := o.CloneHost(recipe)

	if err != nil {
		return err
	}

	cmd := exec.Command("vagrant", "up")
	cmd.Dir = cloneHost
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr

	return cmd.Run()
}

// VagrantStatus queries a Vagrant box clone's status.
func (o Distillery) VagrantStatus(recipe Recipe) (string, error) {
	var outBuffer bytes.Buffer

	cloneHost, err := o.CloneHost(recipe)

	if err != nil {
		return "", err
	}

	cmd := exec.Command("vagrant", "status")
	cmd.Dir = cloneHost
	cmd.Stdout = &outBuffer
	cmd.Stderr = os.Stderr

	if err := cmd.Run(); err != nil {
		return "", err
	}

	return outBuffer.String(), nil
}

// EnsureBooted checks that a Vagrant box is booted.
func (o Distillery) EnsureBootedRecipe(recipe Recipe) error {
	status, err := o.VagrantStatus(recipe)

	if err != nil {
		return err
	}

	if !VagrantStatusRunningPattern.MatchString(status) {
		return o.VagrantUp(recipe)
	}

	return nil
}

// SpinUp checks that a Vagrant box is imported and booted.
func (o Distillery) SpinUpRecipe(recipe Recipe) error {
	if err := o.EnsureCloneRecipe(recipe); err != nil {
		return err
	}

	return o.EnsureBootedRecipe(recipe)
}

// Up ensures that the configured Vagrant boxes are booted.
func (o Distillery) Up() error {
	if err := o.CheckData(); err != nil {
		return err
	}

	for _, recipe := range o.Recipes {
		if err := o.SpinUpRecipe(recipe); err != nil {
			return err
		}
	}

	return nil
}

// Run executes a shell command in a Vagrant box.
func (o Distillery) VagrantRunRecipe(recipe Recipe, step string) error {
	cloneHost, err := o.CloneHost(recipe)

	if err != nil {
		return err
	}

	stepQuoteEscaped := fmt.Sprintf("%q", step)

	cmd := exec.Command("vagrant", "ssh", "-c", stepQuoteEscaped)
	cmd.Dir = cloneHost
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr

	return cmd.Run()
}

// Pour executes a build recipe.
func (o Distillery) PourRecipe(recipe Recipe) error {
	configureEnvironmentVariableStep := recipe.ConfigureEnvironmentVariable(TonixxxArtifactsKey, recipe.ArtifactsGuest())

	var stepsWithEnvironmentVariables []string
	stepsWithEnvironmentVariables = append(stepsWithEnvironmentVariables, configureEnvironmentVariableStep)
	stepsWithEnvironmentVariables = append(stepsWithEnvironmentVariables, recipe.Steps...)

	stepsAggregated := recipe.AggregateSteps(stepsWithEnvironmentVariables)

	if err := o.VagrantRunRecipe(recipe, stepsAggregated); err != nil {
		return err
	}

	return nil
}

// Boil spins up a Vagrant box and executes a build recipe.
//
// Select artifacts may be copied by user steps to ProjectArtifacts path.
//
// If the recipe fails, Boil returns an error.
// Otherwise, Boil returns nil.
func (o Distillery) BoilRecipe(recipe Recipe) error {
	if err := o.SpinUpRecipe(recipe); err != nil {
		return err
	}

	return o.PourRecipe(recipe)
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

		if err := o.BoilRecipe(recipe); err != nil {
			return err
		}

		if err := o.MergeArtifacts(recipe); err != nil {
			return err
		}
	}

	projectArtifacts, err := o.ProjectArtifacts()

	if err != nil {
		return err
	}

	log.Printf("All builds completed successfully. Select artifacts may appear in %s", projectArtifacts)

	return nil
}

// SpinDown halts a Vagrant box.
func (o Distillery) SpinDownRecipe(recipe Recipe) error {
	o.EnsureCloneRecipe(recipe)

	cloneHost, err := o.CloneHost(recipe)

	if err != nil {
		return err
	}

	cmd := exec.Command("vagrant", "halt")
	cmd.Dir = cloneHost

	return cmd.Run()
}


// Down pauses the configured Vagrant boxes.
func (o Distillery) Down() error {
	if err := o.CheckData(); err != nil {
		return err
	}

	for _, recipe := range o.Recipes {
		if err := o.SpinDownRecipe(recipe); err != nil {
			return err
		}
	}

	return nil
}

// MergeArtifacts copies per-recipe build artifacts up to the top-level per-project binary directory.
func (o Distillery) MergeArtifacts(recipe Recipe) error {
	projectArtifacts, err := o.ProjectArtifacts()

	if err != nil {
		return err
	}

	topLevelRecipeArtifactsDirectory := path.Join(projectArtifacts, recipe.Name)

	artifactsRecipePath, err := o.ArtifactsRecipeHost(recipe)

	if err != nil {
		return err
	}

	return copy.Copy(artifactsRecipePath, topLevelRecipeArtifactsDirectory)
}

// RemoveData removes the TonixxxData host directory.
func (o Distillery) RemoveData() error {
	tonixxxHome, err := TonixxxHome()

	if err != nil {
		return err
	}

	return os.RemoveAll(tonixxxHome)
}

// Clean halts a Vagrant box and removes the files from disk.
func (o Distillery) CleanRecipe(recipe Recipe) error {
	if err := o.SpinDownRecipe(recipe); err != nil {
		log.Print(err)
	}

	cloneHost, err := o.CloneHost(recipe)

	if err != nil {
		return err
	}

	return os.RemoveAll(cloneHost)
}

// Clean halts Vagrant boxes and removes the TonixxxData host directory.
func (o Distillery) Clean() error {
	if err := o.CheckData(); err != nil {
		return err
	}

	for _, recipe := range o.Recipes {
		if err := o.CleanRecipe(recipe); err != nil {
			log.Print(err)
		}
	}

	return o.RemoveData()
}
