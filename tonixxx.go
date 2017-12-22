package tonixxx

// Version is semver.
const Version = "0.0.1"

// TonixxxConfigBasename provides the default filename for tonixxx configuration.
const TonixxxConfigBasename = "tonixxx.yaml"

// TonixxxData names the path for housing user build artifacts/ and Vagrant boxes during the build process.
const TonixxxData = ".tonixxx"

// TonixxxArtifactsKey names the guest environment variable for introspecting the tonixxx artifact output directory path.
const TonixxxArtifactsKey = "TONIXXX_ARTIFACTS"

// VagrantSyncedFolder names the guest folder synced to the host.
const VagrantSyncedFolder = "/vagrant"

// VagrantSyncedFolderCOMSPEC names the guest folder synced to the host, with the guest folder in Windows-style backslash notation.
const VagrantSyncedFolderCOMSPEC = "C:\\Users\\vagrant\\Documents\\Vagrant Shares\\"
