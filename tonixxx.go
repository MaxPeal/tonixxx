package tonixxx

import (
	"os/user"
	"path"
)

// Version is semver.
const Version = "0.0.1"

// TonixxxConfigBasename provides the default filename for tonixxx configuration.
const TonixxxConfigBasename = "tonixxx.yaml"

// TonixxxData names the base path for housing tonixxx project metadata, including user build artifacts/ and Vagrant boxes during the build process.
const TonixxxDataBasename = ".tonixxx"

// TonixxxArtifactsKey names the guest environment variable for introspecting the tonixxx artifact output directory path.
const TonixxxArtifactsKey = "TONIXXX_ARTIFACTS"

// VagrantSyncedFolder names the guest folder synced to the host.
const VagrantSyncedFolder = "/vagrant"

// VagrantSyncedFolderCOMSPEC names the guest folder synced to the host, with the guest folder in Windows-style backslash notation.
const VagrantSyncedFolderCOMSPEC = "C:\\Users\\vagrant\\Documents\\Vagrant Shares\\"

// TonixxxHome provides the path to the per-user tonixxx data directory.
func TonixxxHome() (string, error) {
	user, err := user.Current()

	if err != nil {
		return "", err
	}

	return path.Join(user.HomeDir, TonixxxDataBasename), nil
}
