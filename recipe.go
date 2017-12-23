package tonixxx

import (
	"errors"
	"fmt"
	"log"
)

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

	return nil
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
