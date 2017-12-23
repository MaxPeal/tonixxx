package tonixxx

import (
	"errors"
	"fmt"
	"regexp"
)

// BinariesDirectoryBasename provides a location for Vagrant synced folders to emit any artifacts produced during a build. This directory is relative to the recipe's project directory.
const RecipeBinariesDirectoryBasename = "bin"

// VagrantfileBasename refers to the standard configuration file basename for configuring Vagrant boxes.
const VagrantfileBasename = "Vagrantfile"

// VagrantStatusRunningPattern identifies when a Vagrant box is running.
var VagrantStatusRunningPattern *regexp.Regexp = regexp.MustCompile("running")

// RecipeNamePattern constrains names in order to use names as-is for file paths while building a project.
var RecipeNamePattern *regexp.Regexp = regexp.MustCompile("^[a-zA-Z0-9\\.\\-_]+$")

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

// ConfigureEnvironmentVariable generates a shell command for configuring an environment variable.
func (o Recipe) ConfigureEnvironmentVariable(key string, value string) string {
	if o.IsCOMSPEC() {
		return fmt.Sprintf("set %s=\"%s\"", key, value)
	} else {
		return fmt.Sprintf("export %s=\"%s\"", key, value)
	}
}
