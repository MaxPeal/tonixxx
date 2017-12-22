package tonixxx

import (
	"os"
	"path"
)

// Version is semver.
const Version = "0.0.1"

// TONIXXX_DATA names the path for housing user build artifacts/ and Vagrant boxes during the build process.
const TONIXXX_DATA = ".tonixxx"

// TONIXXX_ARTIFACTS_KEY names the guest environment variable for introspecting the tonixxx artifact output directory path.
const TONIXXX_ARTIFACTS_KEY = "TONIXXX_ARTIFACTS"

// VAGRANT_SYNCED_FOLDER names the guest folder synced to the host.
const VAGRANT_SYNCED_FOLDER = "/vagrant"

// VAGRANT_SYNCED_FOLDER_COMSPEC names the guest folder synced to the host, with the guest folder in Windows-style backslash notation.
const VAGRANT_SYNCED_FOLDER_COMSPEC = "C:\\Users\\vagrant\\Documents\\Vagrant Shares\\"

// Step represents an operating system shell command.
type Step = String

// GuestType describes the nature of a Vagrant box.
type GuestType = string

// GuestTypePOSIX identifies a POSIXy Vagrant box.
type GuestTypePOSIX = "POSIX"

// GuestTypeCOMSPEC identifies a Windows Vagrant box.
type GuestTypeCOMSPEC = "COMSPEC"

// Recipe describes the user's build workflow for some target environment.
// By default, recipes assume POSIXy type guests.
type Recipe struct {
	Name string
	Base string
	Type: GuestType
	Steps []Step
}

// ArtifactsHost names the host path where select artifacts may be copied upon building.
func ArtifactsHost() (string, error) {
	dir, err := os.Getwd()

	if err != nil {
		return nil, err
	}

	return path.Join(dir, TONIXXX_DATA), nil
}

func (o Recipe) IsPOSIX() bool {
	return o.GuestType == "POSIX" || o.GuestType == ""
}

func (o Recipe) IsCOMSPEC() bool {
	return o.GuestType == "COMSPEC"
}

// ArtifactsGuest names the guest path for artifacts to be copied during building.
func (o Recipe) ArtifactsGuest() string {
	if o.IsCOMSPEC() {
		return fmt.Sprintf("%s\\%s", VAGRANT_SYNCED_FOLDER_WINDOWS, o.Name)
	} else {
		return fmt.Sprintf("%s/%s", VAGRANT_SYNCED_FOLDER, o.Name)
	}
}

// EnsureClone allocates space for a Vagrant box to be cloned.
func (o Recipe) EnsureClone() error {
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
func (o Recipe) Run(step Step) error {
	panic("Unimplemented")
	// ...
}

// ConfigureEnvironmentVariable generates a shell command for configuring an environment variable.
func (o Recipe) ConfigureEnvironmentVariable(key string, value string) Step {
	if o.IsCOMSPEC() {
		return fmt.Sprintf("set %s=\"%s\"", key, value)
	} else {
		return fmt.Sprintf("export %s=\"%s\"", key, value)
	}
}

// Pour executes a build recipe.
func (o Recipe) Pour() error {
	if err := o.Run(o.ConfigureEnvironmentVariable(TONIXXX_ARTIFACTS_KEY, o.ArtifactsGuest()); err != nil {
		return err
	}

	for _, step := o.Steps {
		if err := o.Run(step); err != nil {
			return err
		}
	}
}

// Boil spins up a Vagrant box and executes a build recipe.
//
// Select artifacts may be copied by user steps to ArtifactsHost path.
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

// Distillery describes a multi-platform build configuration.
type Distillery struct {
	Steps []Step
	Recipes []Recipe
}

// AddData ensures the TONIXXX_DATA host directory is allocated.
func (o Distillery) EnsureData() error {
	if _, err := os.Stat(TONIXXX_DATA); os.IsNotExist(err) {
		return os.Mkdir(TONIXXX_DATA, os.ModeDir)
	}

	return nil
}

// Boil executes configured recipes for a distillery.
func (o Distillery) Boil() error {
	if err := o.EnsureData(); err != nil {
		return err
	}

	for _, recipe := range o.Recipes {
		if err := recipe.Boil(); err != nil {
			return err
		}
	}
}

// RemoveData removes the TONIXXX_DATA host directory.
func (o Distillery) RemoveData() error {
	os.RemoveAll(TONIXXX_DATA)
}

// Clean halts Vagrant boxes and removes the TONIXXX_DATA host directory.
func (o Distillery) Clean() error {
	for _, recipe := o.Recipes {
		if err := o.Clean(); err != nil {
			log.Print(err)
		}
	}

	return o.RemoveData()
}
