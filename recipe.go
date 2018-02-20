package tonixxx

import (
	"errors"
	"fmt"
	"regexp"
	"strings"
)

// RecipeBinariesDirectoryBasename provides a location for Vagrant synced folders to emit any artifacts produced during a build. This directory is relative to the recipe's project directory.
const RecipeBinariesDirectoryBasename = "bin"

// VagrantfileBasename refers to the standard configuration file basename for configuring Vagrant boxes.
const VagrantfileBasename = "Vagrantfile"

// VagrantMetadataDirectory refers to the standard directory for housing internal Vagrant files.
const VagrantMetadataDirectory = ".vagrant"

// VagrantStatusRunningPattern identifies when a Vagrant box is running.
var VagrantStatusRunningPattern = regexp.MustCompile("running")

// RecipeLabelPattern constrains labels in order to use label as-is for file paths while building a project.
var RecipeLabelPattern = regexp.MustCompile(`^[a-zA-Z0-9\.\-_]+$`)

// Recipe describes the user's build workflow for some target environment.
// By default, recipes assume "POSIX".
type Recipe struct {
	Label   string
	Box     string
	Version string
	Type    string
	Steps   []string
}

// Validate applies some semantic checks to a Recipe configuration.
func (o Recipe) Validate() error {
	if !RecipeLabelPattern.MatchString(o.Label) {
		return fmt.Errorf("Recipe label %s fails to match the allowed pattern %s", o.Label, RecipeLabelPattern)
	}

	if o.Box == "" {
		return errors.New("Recipe has empty Vagrant base box")
	}

	if o.Version == "" {
		return errors.New("Recipe has empty Vagrant base box version")
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
		return fmt.Sprintf("%s\\%s", VagrantSyncedFolderCOMSPEC, o.Label)
	}

	return fmt.Sprintf("%s/%s", VagrantSyncedFolder, o.Label)
}

// GenerateVagrantfile supplies the text content of a Vagrantfile for instantiating a recipe.
func (o Recipe) GenerateVagrantfile() string {
	return fmt.Sprintf(
		"Vagrant.configure('2') do |config|\n  config.vm.box = \"%s\"\n  config.vm.box_version = \"%s\"\nend\n",
		o.Box,
		o.Version,
	)
}

// ConfigureEnvironmentVariable generates a shell command for configuring an environment variable.
func (o Recipe) ConfigureEnvironmentVariable(key string, value string) string {
	if o.IsCOMSPEC() {
		return fmt.Sprintf("set %s=\"%s\"", key, value)
	}

	return fmt.Sprintf("export %s=\"%s\"", key, value)
}

// AggregateSteps constructs a logical whole command out of subcommands.
func (o Recipe) AggregateSteps(steps []string) string {
	if o.IsCOMSPEC() {
		return strings.Join(steps, " & ")
	}

	return strings.Join(steps, " && ")
}
