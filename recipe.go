package tonixxx

import (
	"errors"
	"fmt"
	"log"
	"os"
	"path"
	"regexp"
)

// BinariesDirectoryBasename provides a location for Vagrant synced folders to emit any artifacts produced during a build. This directory is relative to the recipe's project directory.
const RecipeBinariesDirectoryBasename = "bin"

// VagrantfileBasename refers to the standard configuration file basename for configuring Vagrant boxes.
const VagrantfileBasename = "Vagrantfile"

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
	contentVagrantfile = fmt.Sprintf("Vagrant.configure('2') do |config|\n  config.vm.box = \"%s\"\nend", o.Base)

	vagrantfilePath, err := o.VagrantfilePath()

	if err != nil {
		return err
	}

	// Write Vagrantfile...


	panic("Unimplemented")
	// ...
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

	// generate Vagrantfile in the ClonePath directory

	// `vagrant up` (in the ClonePath directory)
	// ...

	panic("Unimplemented")
	// ...
}

// EnsureImported checks that a Vagrant box is imported.
// If not, EnsureImported attempts to download the box from the public Vagrant Cloud repository.
func (o Recipe) EnsureImported() error {
	panic("Unimplemented")
	// ...
}

// EnsureBooted checks that a Vagrant box is booted.
func (o Recipe) EnsureBooted() error {
	panic("Unimplemented")
	// ...
}

// SpinUp checks that a Vagrant box is imported and booted.
func (o Recipe) SpinUp() error {
	if err := o.EnsureClone(); err != nil {
		return err
	}

	if err := o.EnsureImported(); err != nil {
		return err
	}

	return o.EnsureBooted()
}

// Run executes a shell command in a Vagrant box.
func (o Recipe) Run(step string) error {
	panic("Unimplemented")
	// ...
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
	if err := o.Run(o.ConfigureEnvironmentVariable(TonixxxArtifactsKey, o.ArtifactsGuest())); err != nil {
		return err
	}

	for _, step := range o.Steps {
		if err := o.Run(step); err != nil {
			return err
		}
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
	panic("Unimplemented")
	// ...
}

// MergeArtifacts copies per-recipe build artifacts up to the top-level per-project binary directory.
func (o Recipe) MergeArtifacts(projectArtifactsPath string) error {
	topLevelRecipeArtifactsDirectory = path.Join(projectArtifactsPath, o.Name)

	return copy.Copy(o.ArtifactsHost, topLevelRecipeArtifactsDirectory)
}

// Remove deletes a Vagrant box clone from disk.
func (o Recipe) Remove() error {
	panic("Unimplemented")
	// ...
}

// Clean halts a Vagrant box and removes the files from disk.
func (o Recipe) Clean() error {
	if err := o.SpinDown(); err != nil {
		log.Print(err)
	}

	return o.Remove()
}
