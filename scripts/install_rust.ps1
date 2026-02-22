[Net.ServicePointManager]::SecurityProtocol = [Net.SecurityProtocolType]::Tls12
$dest = Join-Path $env:TEMP "rustup-init.exe"
Write-Host "Downloading rustup-init.exe..."
(New-Object System.Net.WebClient).DownloadFile("https://static.rust-lang.org/rustup/dist/x86_64-pc-windows-msvc/rustup-init.exe", $dest)
Write-Host "Downloaded to: $dest"
Write-Host "Running rustup-init with default options..."
& $dest -y --default-toolchain stable
Write-Host "Done. Refreshing PATH..."
$env:Path = [System.Environment]::GetEnvironmentVariable("Path", "User") + ";" + [System.Environment]::GetEnvironmentVariable("Path", "Machine")
& "$env:USERPROFILE\.cargo\bin\cargo.exe" --version
& "$env:USERPROFILE\.cargo\bin\rustc.exe" --version
