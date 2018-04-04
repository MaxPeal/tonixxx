package tonixxx

import (
	"errors"
	"fmt"
	"regexp"
	"strings"
)

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
	Label              string
	Box                string
	Version            string
	GuestType          string
	ArtifactsGuestPath string
	Steps              []string
}

// Validate applies some semantic checks to a Recipe configuration.
func (o Recipe) Validate() error {
	if !RecipeLabelPattern.MatchString(o.Label) {
		return fmt.Errorf("Recipe label %s fails to match the allowed pattern %s", o.Label, RecipeLabelPattern)
	}

	if o.Box == "" {
		return errors.New("Recipe has empty Vagrant base box")
	}

	return nil
}

// SyncedFolderGuestPath names the guest path for artifacts to be copied during building.
func (o Recipe) SyncedFolderGuestPath() string {
	if o.ArtifactsGuestPath != "" {
		return o.ArtifactsGuestPath
	}

	switch o.GuestType {
	case GuestTypeCygwin:
		return VagrantSyncedFolderCygwin
	case GuestTypeSmartOSGZ:
		return VagrantSyncedFolderSmartOSGZ
	default:
		return VagrantSyncedFolderPOSIX
	}
}

// GenerateVagrantfile supplies the text content of a Vagrantfile for instantiating a recipe.
func (o Recipe) GenerateVagrantfile() string {
	vagrantfileContent := fmt.Sprintf(
		"Vagrant.configure('2') do |config|\n  config.vm.box = \"%s\"\n",
		o.Box,
	)

	if o.Version != "" {
		vagrantfileContent += fmt.Sprintf("  config.vm.box_version = \"%s\"\n", o.Version)
	}

	vagrantfileContent += "\nend"

	return vagrantfileContent
}

// ConfigureEnvironmentVariable generates a shell command for configuring an environment variable.
func (o Recipe) ConfigureEnvironmentVariable(key string, value string) string {
	return fmt.Sprintf("export %s=\"%s\"", key, value)
}

// AggregateSteps constructs a logical whole command out of subcommands.
func (o Recipe) AggregateSteps(steps []string) string {
	return strings.Join(steps, " && ")
}
