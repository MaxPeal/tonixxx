package tonixxx

import (
	"os/user"
	"path"
)

// Version is semver.
const Version = "0.0.2-dev"

// TonixxxConfigBasename provides the default filename for tonixxx configuration.
const TonixxxConfigBasename = "tonixxx.yaml"

// TonixxxDataBasename describes the base path for housing tonixxx project metadata, including user build artifacts/ and Vagrant boxes during the build process.
const TonixxxDataBasename = ".tonixxx"

// TonixxxSyncKey names the guest environment variable for introspecting a VM's synced folder guest path.
const TonixxxSyncKey = "TONIXXX_SYNC"

// VagrantSyncedFolderPOSIX names the guest folder synced with the host for POSIXy VMs.
const VagrantSyncedFolderPOSIX = "/vagrant"

// VagrantSyncedFolderCygwin names the guest folder synced with the host, with the guest folder in UNIX-style forwardslash notation.
const VagrantSyncedFolderCygwin = "/c/vagrant"

// VagrantSyncedFolderSmartOSGZ names the guest folder synced with the host for SmartOS global zone VMs.
const VagrantSyncedFolderSmartOSGZ = "/opt/vagrant"

// DataHome provides the path to the per-user tonixxx data directory.
func DataHome() (string, error) {
	user, err := user.Current()

	if err != nil {
		return "", err
	}

	return path.Join(user.HomeDir, TonixxxDataBasename), nil
}
