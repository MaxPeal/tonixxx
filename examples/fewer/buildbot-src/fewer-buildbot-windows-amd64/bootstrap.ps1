$ErrorActionPreference = "Stop"

#
# Install cmake, add to session PATH, persist PATH.
#
choco install -force -y cmake
$cmakePath = "C:\Program Files\CMake\bin"
$env:PATH += ";$cmakePath"
$userPath = (Get-ItemProperty "Registry::HKEY_CURRENT_USER\Environment").PATH
Set-ItemProperty -Path "Registry::HKEY_CURRENT_USER\Environment" -Name PATH -Value "$userPath;$cmakePath"

#
# Install cppcheck, add to session PATH, persist PATH.
#
choco install -force -y cppcheck
$cppcheckPath = "C:\Program Files\Cppcheck"
$env:PATH += ";$cppcheckPath"
$userPath = (Get-ItemProperty "Registry::HKEY_CURRENT_USER\Environment").PATH
Set-ItemProperty -Path "Registry::HKEY_CURRENT_USER\Environment" -Name PATH -Value "$userPath;$cppcheckPath"

#
# Install doxygen, add to session PATH, persist PATH.
#
choco install -force -y doxygen.install
$doxygenPath = "C:\Program Files\doxygen\bin"
$env:PATH += ";$doxygenPath"
$userPath = (Get-ItemProperty "Registry::HKEY_CURRENT_USER\Environment").PATH
Set-ItemProperty -Path "Registry::HKEY_CURRENT_USER\Environment" -Name PATH -Value "$userPath;$doxygenPath"

#
# Install graphviz, add to session PATH, persist PATH.
#
choco install -force -y graphviz
$doxygenPath = "C:\Program Files\Graphviz 2.44.1\bin"
$env:PATH += ";$doxygenPath"
$userPath = (Get-ItemProperty "Registry::HKEY_CURRENT_USER\Environment").PATH
Set-ItemProperty -Path "Registry::HKEY_CURRENT_USER\Environment" -Name PATH -Value "$userPath;$doxygenPath"
dot -c

# Update PowerShell SSL protocols
[Net.ServicePointManager]::SecurityProtocol = "tls12, tls11, tls"

#
# Install Python dependencies.
#
choco install -force -y python3
$pythonPath = "C:\Python39"
$pythonScriptPath = "C:\Python39\Scripts"
$env:PATH += ";$pythonPath;$pythonScriptPath"
$userPath = (Get-ItemProperty "Registry::HKEY_CURRENT_USER\Environment").PATH
Set-ItemProperty -Path "Registry::HKEY_CURRENT_USER\Environment" -Name PATH -Value "$userPath;$pythonPath;$pythonScriptPath"
python -m pip install -U pip
pip3 install -r C:\Users\vagrant\AppData\Local\Temp\requirements-dev.txt