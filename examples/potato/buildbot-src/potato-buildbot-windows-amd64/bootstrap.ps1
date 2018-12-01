# Install cmake, add to session PATH, persist PATH.
choco install -force -y cmake
$cmakePath = "C:\Program Files\CMake\bin"
$env:PATH += ";$cmakePath"
$userPath = (Get-ItemProperty "Registry::HKEY_CURRENT_USER\Environment").PATH
Set-ItemProperty -Path "Registry::HKEY_CURRENT_USER\Environment" -Name PATH -Value "$userPath;$cmakePath"

dub fetch dscanner