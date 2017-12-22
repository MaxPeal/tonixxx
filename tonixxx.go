package tonixxx

// Version is semver.
const Version = "0.0.1"

// TONIXXX_CONFIG_BASENAME provides the default filename for tonixxx configuration.
const TONIXXX_CONFIG_BASENAME = "tonixxx.yaml"

// TONIXXX_DATA names the path for housing user build artifacts/ and Vagrant boxes during the build process.
const TONIXXX_DATA = ".tonixxx"

// TONIXXX_ARTIFACTS_KEY names the guest environment variable for introspecting the tonixxx artifact output directory path.
const TONIXXX_ARTIFACTS_KEY = "TONIXXX_ARTIFACTS"

// VAGRANT_SYNCED_FOLDER names the guest folder synced to the host.
const VAGRANT_SYNCED_FOLDER = "/vagrant"

// VAGRANT_SYNCED_FOLDER_COMSPEC names the guest folder synced to the host, with the guest folder in Windows-style backslash notation.
const VAGRANT_SYNCED_FOLDER_COMSPEC = "C:\\Users\\vagrant\\Documents\\Vagrant Shares\\"
