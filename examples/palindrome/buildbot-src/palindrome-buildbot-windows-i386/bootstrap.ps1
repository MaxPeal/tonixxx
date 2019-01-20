$ErrorActionPreference = "Stop"

# Install cmake, add to session PATH, persist PATH.
choco install -force -y cmake
$cmakePath = "C:\Program Files\CMake\bin"
$env:PATH += ";$cmakePath"
$userPath = (Get-ItemProperty "Registry::HKEY_CURRENT_USER\Environment").PATH
Set-ItemProperty -Path "Registry::HKEY_CURRENT_USER\Environment" -Name PATH -Value "$userPath;$cmakePath"

# Install cppcheck, add to session PATH, persist PATH.
choco install -force -y cppcheck
$cppcheckPath = "C:\Program Files\Cppcheck"
$env:PATH += ";$cppcheckPath"
$userPath = (Get-ItemProperty "Registry::HKEY_CURRENT_USER\Environment").PATH
Set-ItemProperty -Path "Registry::HKEY_CURRENT_USER\Environment" -Name PATH -Value "$userPath;$cppcheckPath"

# Update PowerShell SSL protocols
[Net.ServicePointManager]::SecurityProtocol = "tls12, tls11, tls"

choco install -force -y python3
$pythonPath = "C:\Python37"
$pythonScriptPath = "C:\Python37\Scripts"
$env:PATH += ";$pythonPath;$pythonScriptPath"
$userPath = (Get-ItemProperty "Registry::HKEY_CURRENT_USER\Environment").PATH
Set-ItemProperty -Path "Registry::HKEY_CURRENT_USER\Environment" -Name PATH -Value "$userPath;$pythonPath;$pythonScriptPath"
python -m pip install -U pip
pip3 install cpplint