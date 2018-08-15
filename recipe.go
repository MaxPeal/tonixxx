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
	"strings"

	"github.com/mcandre/popcopy"
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
	// Label (required) provides a descriptor for the artifact produced by this recipe,
	// meaningful to a particular project to distinguish these artifacts
	// from those artifacts produced by sister recipes.
	//
	// Example: "minix-i386"
	Label string

	// Box (required) describes a Vagrant box name.
	//
	// Example: "mcandre/minix"
	Box string

	// Version (optional) describes a Vagrant box version.
	//
	// Example: "0.0.1"
	Version string

	// GuestType (optional) describes the kind of guest OS.
	//
	// Defaults to GuestTypePOSIX.
	//
	// Example: "Cygwin"
	GuestType string

	// ArtifactsGuestPath (optional) names a directory path to place source files
	// into the guest before building.
	//
	// Defaults to a Vagrant rsync path based on known GuestType's.
	//
	// Example: "/vagrant"
	ArtifactsGuestPath string

	// PreSteps (optional) lists any guest commands to execute before normal Steps.
	//
	// Example: []string{"sudo yum update"}
	PreSteps []string

	// Steps (optional) lists the guest commands for building artifacts.
	//
	// Example: []string{"cd \"$TONIXXX_SYNC\"", "make"}
	Steps []string

	// PostSteps (optional) lists any guest commands to execute after normal Steps.
	//
	// Example: []string{"sudo yum clean all"}
	PostSteps []string

	// projectData (injected) caches a project's data directory.
	projectData string

	// projectArtifacts (injected) caches a project's unified artifact output directory.
	projectArtifacts string
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
	case GuestTypeHaiku:
		return VagrantSyncedFolderHaiku
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

// CloneHost supplies the directory path inside ~/.tonixxx in which a recipe's Vagrant clone resides.
func (o Recipe) CloneHost() string {
	return path.Join(o.projectData, o.Label)
}

// ArtifactHost supplies the directory where recipe-relative artifacts are
// first copied upon builds.
func (o Recipe) ArtifactHost(effectiveOutputDirectory string) string {
	return path.Join(o.CloneHost(), effectiveOutputDirectory)
}

// EnsureSourceCopy ensures that project source files are copied to the clone host.
func (o Recipe) EnsureSourceCopy() error {
	cwd, err := os.Getwd()

	if err != nil {
		return err
	}

	return popcopy.Copy(
		cwd,
		o.CloneHost(),
		ImplicitRecipeExclusions,
	)
}

// VagrantfilePath queries the path to this recipe's Vagrantfile.
func (o Recipe) VagrantfilePath() string {
	return path.Join(o.CloneHost(), VagrantfileBasename)
}

// EnsureVagrantfile ensures that this recipe has a Vagrantfile generated.
func (o Recipe) EnsureVagrantfile() error {
	if err := o.EnsureSourceCopy(); err != nil {
		return err
	}

	return ioutil.WriteFile(
		o.VagrantfilePath(),
		[]byte(o.GenerateVagrantfile()),
		0644,
	)
}

// VagrantStatus reports an instance description.
func (o Recipe) VagrantStatus() (string, error) {
	if err := o.EnsureVagrantfile(); err != nil {
		return "", err
	}

	var outBuffer bytes.Buffer

	cmd := exec.Command("vagrant", "status")
	cmd.Env = os.Environ()
	cmd.Dir = o.CloneHost()
	cmd.Stdout = &outBuffer
	cmd.Stderr = os.Stderr

	err := cmd.Run()

	return outBuffer.String(), err
}

// IsRunning queries a Vagrant instance for running status.
func (o Recipe) IsRunning() (bool, error) {
	status, err := o.VagrantStatus()

	if err != nil {
		return false, err
	}

	return VagrantStatusRunningPattern.MatchString(status), nil
}

// VagrantUp ensures a recipe is booted.
func (o Recipe) VagrantUp() error {
	cmd := exec.Command("vagrant", "up")
	cmd.Env = os.Environ()
	cmd.Dir = o.CloneHost()
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr

	return cmd.Run()
}

// VagrantRsync copies host-clone source files into a guest in preparation for builds.
func (o Recipe) VagrantRsync() error {
	cmd := exec.Command("vagrant", "rsync")
	cmd.Env = os.Environ()
	cmd.Dir = o.CloneHost()
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr

	return cmd.Run()
}

// EnsureRsync ensures that a Vagrant instance is running and
// that the host source is copied to the guest.
func (o Recipe) EnsureRsync() error {
	if err := o.EnsureVagrantfile(); err != nil {
		return err
	}

	running, err := o.IsRunning()

	if err != nil {
		return err
	}

	if running {
		return o.VagrantRsync()
	}

	return o.VagrantUp()
}

// VagrantSSH executes a guest command.
func (o Recipe) VagrantSSH(step string, debug bool) error {
	cmd := exec.Command("vagrant", "ssh", "--no-tty", "-c", step)
	cmd.Env = os.Environ()
	cmd.Dir = o.CloneHost()
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr

	if debug {
		log.Printf("Executing command: %v", cmd)
	}

	return cmd.Run()
}

// VagrantRsyncBack copies guest artifacts back to a host-clone directory.
func (o Recipe) VagrantRsyncBack() error {
	cmd := exec.Command("vagrant", "rsync-back")
	cmd.Env = os.Environ()
	cmd.Dir = o.CloneHost()
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr

	return cmd.Run()
}

// MergeArtifacts copies recipe-relative build artifacts to a common,
// project-relative file tree.
func (o Recipe) MergeArtifacts(effectiveOutputDirectory string) error {
	topLevelRecipeArtifactsDirectory := path.Join(o.projectArtifacts, o.Label)
	artifactsRecipePath := o.ArtifactHost(effectiveOutputDirectory)
	return popcopy.Copy(artifactsRecipePath, topLevelRecipeArtifactsDirectory, []*regexp.Regexp{})
}

// Boil executes build steps for a recipe.
//
// The build steps may include instructions to copy select build artifacts
// to the SyncKey path. After a successful run of the build steps,
// any files in the SyncKey path are copied back to the host.
func (o Recipe) Boil(effectiveOutputDirectory *string, debug bool) error {
	if err := o.EnsureRsync(); err != nil {
		return err
	}

	configureSyncedFolderEnvVarStep := o.ConfigureEnvironmentVariable(SyncKey, o.SyncedFolderGuestPath())

	var stepsWithEnvironmentVariables []string
	stepsWithEnvironmentVariables = append(stepsWithEnvironmentVariables, configureSyncedFolderEnvVarStep)
	stepsWithEnvironmentVariables = append(stepsWithEnvironmentVariables, o.PreSteps...)
	stepsWithEnvironmentVariables = append(stepsWithEnvironmentVariables, o.Steps...)
	stepsWithEnvironmentVariables = append(stepsWithEnvironmentVariables, o.PostSteps...)

	stepsAggregated := o.AggregateSteps(stepsWithEnvironmentVariables)

	if err := o.VagrantSSH(stepsAggregated, debug); err != nil {
		return err
	}

	if effectiveOutputDirectory != nil {
		if err := o.VagrantRsyncBack(); err != nil {
			return err
		}

		return o.MergeArtifacts(*effectiveOutputDirectory)
	}

	return nil
}

// VagrantDown ensures a recipe is halted.
func (o Recipe) VagrantDown() error {
	if err := o.EnsureVagrantfile(); err != nil {
		return err
	}

	cmd := exec.Command("vagrant", "halt")
	cmd.Env = os.Environ()
	cmd.Dir = o.CloneHost()

	return cmd.Run()
}

// Destroy removes a recipe's Vagrant instance.
func (o Recipe) Destroy() error {
	if err := o.EnsureVagrantfile(); err != nil {
		return err
	}

	cloneHost := o.CloneHost()

	cmd := exec.Command("vagrant", "destroy", "-f")
	cmd.Env = os.Environ()
	cmd.Dir = cloneHost

	if err := cmd.Run(); err != nil {
		return err
	}

	vagrantMetadataDir := path.Join(cloneHost, VagrantMetadataDirectory)

	return os.RemoveAll(vagrantMetadataDir)
}

// Clean destroys a Vagrant instance and
// removes the per-recipe host directory.
func (o Recipe) Clean() error {
	if err := o.Destroy(); err != nil {
		log.Print(err)
	}

	return os.RemoveAll(o.CloneHost())
}
