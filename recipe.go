package tonixxx

import (
	"errors"
	"fmt"
	"io/ioutil"
	"log"
	"os"
	"path"
	"regexp"
)

// BinariesDirectoryBasename provides a location for Vagrant synced folders to emit any artifacts produced during a build. This directory is relative to the recipe's project directory.
const RecipeBinariesDirectoryBasename = "bin"

// VagrantfileBasename refers to the standard configuration file basename for configuring Vagrant boxes.
const VagrantfileBasename = "Vagrantfile"

// VagrantStatusRunningPattern identifies when a Vagrant box is running.
var VagrantStatusRunningPattern *regexp.Regexp = regexp.MustCompile("running")

// RecipeNamePattern constrains names in order to use names as-is for file paths while building a project.
var RecipeNamePattern *regexp.Regexp = regexp.MustCompile("^[a-zA-Z\\.\\-_]+$")

// Recipe describes the user's build workflow for some target environment.
// By default, recipes assume "POSIX".
type Recipe struct {
	Name  string
	Base  string
	Type  string
	Steps []string
}

// Validate applies some semantic checks to a Recipe configuration.
func (o Recipe) Validate() error {
	if o.Name == "" {
		return errors.New("Recipe has empty name")
	}

	if !RecipeNamePattern.MatchString(o.Name) {
		return errors.New(fmt.Sprintf("Recipe name %s contains elements outside of the allowed pattern %s", o.Name, RecipeNamePattern))
	}

	if o.Base == "" {
		return errors.New("Recipe has empty Vagrant base box")
	}

	return nil
}

// IsCOMSPEC checks whether a recipe is configured as COMSPEC (Windows) type.
func (o Recipe) IsCOMSPEC() bool {
	return o.Type == "COMSPEC"
}

// IsPOSIX checks whether a recipe is configured as POSIX type.
// Recipes default to POSIX type if unspecified.
func (o Recipe) IsPOSIX() bool {
	return o.Type == "POSIX" || o.Type == ""
}

// ArtifactsGuest names the guest path for artifacts to be copied during building.
func (o Recipe) ArtifactsGuest() string {
	if o.IsCOMSPEC() {
		return fmt.Sprintf("%s\\%s", VagrantSyncedFolderCOMSPEC, o.Name)
	}

	return fmt.Sprintf("%s/%s", VagrantSyncedFolder, o.Name)
}

// VagrantHostDirectory provides the path to a recipe's Vagrant directory.
func (o Recipe) VagrantHostDirectory() (string, error) {
	projectData, err := ProjectData()

	if err != nil {
		return "", nil
	}

	return paths.Join(projectData, o.Name), nil
}

func (o Recipe) VagrantfilePath() (string, error) {
	vagrantHostDir, err := o.VagrantHostDirectory()

	if err != nil {
		return "", err
	}

	return paths.Join(vagrantHostDir, VagrantfileBasename), nil
}

// Generate a basic Vagrantfile from the recipe base box url.
func (o Recipe) EnsureVagrantfile() error {
	contentVagrantfile := fmt.Sprintf("Vagrant.configure('2') do |config|\n  config.vm.box = \"%s\"\nend", o.Base)

	vagrantfilePath, err := o.VagrantfilePath()

	if err != nil {
		return err
	}

	contentVagrantfileBytes := []byte(contentVagrantfile)

	return ioutil.WriteFile(vagrantfilePath, contentVagrantfileBytes, 0644)
}

// ArtifactsHost provides a recipe relative output directory.
// If all builds in a distillery are successful,
// then these artifacts are copied to a project relative directory.
func (o Recipe) ArtifactsHost() (string, error) {
	vagrantHostDir, err := VagrantHostDirectory()

	if err != nil {
		return "", err
	}

	return paths.Join(vagrantHostDir, RecipeBinariesDirectoryBasename)
}

func (o Recipe) EnsureArtifactsHost() error {
	return Mkdir(o.ArtifactsHost())
}

func (o Recipe) CloneHost() (string, error) {
	dir, err := os.Getwd()

	if err != nil {
		return "", err
	}

	return path.Join(dir, o.Name), nil
}

func (o Recipe) EnsureClonePath() error {
	pth, err := o.CloneHost()

	if err != nil {
		return err
	}

	return EnsureDirectory(pth)
}

// VagrantStatus queries a Vagrant box clone's status.
func (o Recipe) VagrantStatus() (string, error) {
	var outBuffer bytes.Buffer

	cmd := exec.Command("vagrant", "status")
	cmd.Dir = o.CloneHost()
	cmd.Stdout = &outBuffer

	if err := cmd.Run(); err != nil {
		return "", error
	}

	return outBuffer.String(), nil
}

// VagrantUp boots a Vagrant box.
func (o Recipe) VagrantUp() error {
	cmd := exec.Command("vagrant", "up")
	cmd.Dir = o.CloneHost()
	return cmd.Run()
}

// EnsureClone allocates space for a Vagrant box to be cloned.
func (o Recipe) EnsureClone() error {
	if err := o.EnsureClonePath(); err != nil {
		return err
	}

	cwd, err := os.Getwd()

	if err != nil {
		return err
	}

	// Copy source files into the right Vagrant clone directory.
	if err := copy.Copy(cwd, o.CloneHost()); err != nil {
		return err
	}

	if err := o.EnsureArtifactsHost(); err != nil {
		return err
	}

	if err := o.EnsureVagrantfile(); err != nil {
		return err
	}
}

// EnsureBooted checks that a Vagrant box is booted.
func (o Recipe) EnsureBooted() error {
	status, err := o.VagrantStatus()

	if err != nil {
		return err
	}

	if !VagrantStatusRunningPattern.MatchString(status) {
		return os.VagrantUp()
	}
}

// SpinUp checks that a Vagrant box is imported and booted.
func (o Recipe) SpinUp() error {
	if err := o.EnsureClone(); err != nil {
		return err
	}

	return o.EnsureBooted()
}

// Run executes a shell command in a Vagrant box.
func (o Recipe) VagrantRun(step string) error {
	stepQuoteEscaped := fmt.Sprintf("%q", step)

	cmd := exec.Command("vagrant", "ssh", "-c", stepQuoteEscaped)
	cmd.Dir = o.CloneHost()
	return cmd.Run()
}

// ConfigureEnvironmentVariable generates a shell command for configuring an environment variable.
func (o Recipe) ConfigureEnvironmentVariable(key string, value string) string {
	if o.IsCOMSPEC() {
		return fmt.Sprintf("set %s=\"%s\"", key, value)
	} else {
		return fmt.Sprintf("export %s=\"%s\"", key, value)
	}
}

// Pour executes a build recipe.
func (o Recipe) Pour() error {
	var stepAccumulator bytes.Buffer
	stepAccumulator.WriteString(o.ConfigureEnvironmentVariable(TonixxxArtifactsKey, o.ArtifactsGuest()))
	stepAccumulator.WriteString("\n")
	stepAccumulator.WriteString(strings.Join(o.Steps, "\n"))

	stepsAggregate = stepAccumulator.String()

	if err := o.VagrantRun(stepsAggregate); err != nil {
		return err
	}

	log.Printf("A build completed. Select artifacts may appear in %s", ProjectArtifacts())

	return nil
}

// Boil spins up a Vagrant box and executes a build recipe.
//
// Select artifacts may be copied by user steps to ProjectArtifacts path.
//
// If the recipe fails, Boil returns an error.
// Otherwise, Boil returns nil.
func (o Recipe) Boil() error {
	if err := o.SpinUp(); err != nil {
		return err
	}

	return o.Pour()
}

// SpinDown halts a Vagrant box.
func (o Recipe) SpinDown() error {
	o.EnsureClone()

	cmd := exec.Command("vagrant", "halt")
	cmd.Dir = o.CloneHost()
}

// MergeArtifacts copies per-recipe build artifacts up to the top-level per-project binary directory.
func (o Recipe) MergeArtifacts(projectArtifactsPath string) error {
	topLevelRecipeArtifactsDirectory = path.Join(projectArtifactsPath, o.Name)

	return copy.Copy(o.ArtifactsHost, topLevelRecipeArtifactsDirectory)
}

// Clean halts a Vagrant box and removes the files from disk.
func (o Recipe) Clean() error {
	if err := o.SpinDown(); err != nil {
		log.Print(err)
	}

	return os.RemoveAll(o.CloneHost())
}
