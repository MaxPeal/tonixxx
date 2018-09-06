package tonixxx

import (
	"os/user"
	"path"
	"regexp"
)

// Version is semver.
const Version = "0.0.9"

// ConfigBasename provides the default filename for tonixxx configuration.
const ConfigBasename = "tonixxx.yaml"

// DataBasename describes the base path for housing tonixxx project metadata, including user build artifacts/ and Vagrant boxes during the build process.
const DataBasename = ".tonixxx"

// BuildbotsBasename describes the base path for housing buildbot provisioning files,
// which aids tonixxx in excluding these (potentially large files)
// from recipe clones during project builds with tonixxx up and tonixxx boil.
const BuildbotsBasename = "buildbot-src"

// ImplicitRecipeExclusions names file patterns to be automatically ignored
// when copying a tonixxx project's files to per-recipe directories.
var ImplicitRecipeExclusions = []*regexp.Regexp{
	regexp.MustCompile(BuildbotsBasename),
	regexp.MustCompile("\\.envrc"),
}

// DefaultOutputDirectory locates project-wide binary aggregation at the end of a build, relative to the tonixxx per-project metadata directory.
const DefaultOutputDirectory = "bin"

// DefaultMaxRunningRecipes constrains the number of simultaneously running Vagrant boxes,
// in order to reduce resource strain on limited host hardware.
const DefaultMaxRunningRecipes = 1

// MaxRecipesKey names the host environment variable for restricting
// the number of simultaneously running recipes,
// in order to reduce resource strain on limited host hardware.
//
const MaxRecipesKey = "TONIXXX_MAX_RECIPES"

// SyncKey names the guest environment variable for introspecting a VM's synced folder guest path.
const SyncKey = "TONIXXX_SYNC"

// VagrantSyncedFolderPOSIX names the guest folder synced with the host for POSIXy VMs.
const VagrantSyncedFolderPOSIX = "/vagrant"

// VagrantSyncedFolderCygwin names the guest folder synced with the host, with the guest folder in UNIX-style forwardslash notation.
const VagrantSyncedFolderCygwin = "/c/vagrant"

// VagrantSyncedFolderSmartOSGZ names the guest folder synced with the host for SmartOS global zone VMs.
const VagrantSyncedFolderSmartOSGZ = "/opt/vagrant"

// VagrantSyncedFolderHaiku names the guest folder synced with the host for Haiku OS VMs.
const VagrantSyncedFolderHaiku = "/boot/vagrant-src"

// DataHome provides the path to the per-user tonixxx data directory.
func DataHome() (string, error) {
	user, err := user.Current()

	if err != nil {
		return "", err
	}

	return path.Join(user.HomeDir, DataBasename), nil
}
