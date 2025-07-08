# setup-fabric-minimal.ps1 - Creates minimal Hyperledger Fabric configuration for testing

Write-Host "Setting up minimal Fabric configuration..." -ForegroundColor Green

# Create directory structure
Write-Host "Creating directory structure..." -ForegroundColor Yellow
$directories = @(
    "blockchain\config",
    "blockchain\crypto-config\peerOrganizations\org1.example.com\peers\peer0.org1.example.com\msp\admincerts",
    "blockchain\crypto-config\peerOrganizations\org1.example.com\peers\peer0.org1.example.com\msp\cacerts",
    "blockchain\crypto-config\peerOrganizations\org1.example.com\peers\peer0.org1.example.com\msp\keystore",
    "blockchain\crypto-config\peerOrganizations\org1.example.com\peers\peer0.org1.example.com\msp\signcerts",
    "blockchain\crypto-config\peerOrganizations\org1.example.com\peers\peer0.org1.example.com\msp\tlscacerts",
    "scripts",
    "cobol_programs",
    "adapters",
    "data",
    "logs",
    "pipes"
)

foreach ($dir in $directories) {
    New-Item -ItemType Directory -Force -Path $dir | Out-Null
    Write-Host "  Created: $dir" -ForegroundColor Gray
}

# Create MSP config.yaml
Write-Host "`nCreating MSP config.yaml..." -ForegroundColor Yellow
$mspConfig = @"
NodeOUs:
  Enable: true
  ClientOUIdentifier:
    Certificate: cacerts/ca.org1.example.com-cert.pem
    OrganizationalUnitIdentifier: client
  PeerOUIdentifier:
    Certificate: cacerts/ca.org1.example.com-cert.pem
    OrganizationalUnitIdentifier: peer
  AdminOUIdentifier:
    Certificate: cacerts/ca.org1.example.com-cert.pem
    OrganizationalUnitIdentifier: admin
  OrdererOUIdentifier:
    Certificate: cacerts/ca.org1.example.com-cert.pem
    OrganizationalUnitIdentifier: orderer
"@

$mspConfig | Out-File -FilePath "blockchain\crypto-config\peerOrganizations\org1.example.com\peers\peer0.org1.example.com\msp\config.yaml" -Encoding UTF8

# Check if OpenSSL is available
$opensslPath = Get-Command openssl -ErrorAction SilentlyContinue
if (-not $opensslPath) {
    Write-Host "`nOpenSSL not found in PATH. Creating dummy certificates for testing..." -ForegroundColor Yellow
    
    # Create dummy certificate files for testing
    $dummyCert = @"
-----BEGIN CERTIFICATE-----
MIIFazCCA1OgAwIBAgIUXkqGHuiPOiXn2dnotLEO1d4E7F4wDQYJKoZIhvcNAQEL
BQAwRTELMAkGA1UEBhMCVVMxEzARBgNVBAgMClNvbWUtU3RhdGUxITAfBgNVBAoM
GEludGVybmV0IFdpZGdpdHMgUHR5IEx0ZDAeFw0yNDAxMDEwMDAwMDBaFw0yNTAx
MDEwMDAwMDBaMEUxCzAJBgNVBAYTAlVTMRMwEQYDVQQIDApTb21lLVN0YXRlMSEw
HwYDVQQKDBhJbnRlcm5ldCBXaWRnaXRzIFB0eSBMdGQwggIiMA0GCSqGSIb3DQEB
AQUAA4ICDwAwggIKAoICAQDL9P0HIZMg4zgbWrj05XGLEBMKvBfRSiJl8VgWqyWq
-----END CERTIFICATE-----
"@
    
    $dummyKey = @"
-----BEGIN PRIVATE KEY-----
MIIEvQIBADANBgkqhkiG9w0BAQEFAASCBKcwggSjAgEAAoIBAQC/W0LZWl3wqvNh
-----END PRIVATE KEY-----
"@
    
    # Save dummy files
    $certPath = "blockchain\crypto-config\peerOrganizations\org1.example.com\peers\peer0.org1.example.com\msp\signcerts\peer0.org1.example.com-cert.pem"
    $keyPath = "blockchain\crypto-config\peerOrganizations\org1.example.com\peers\peer0.org1.example.com\msp\keystore\priv_sk"
    
    $dummyCert | Out-File -FilePath $certPath -Encoding UTF8
    $dummyKey | Out-File -FilePath $keyPath -Encoding UTF8
    
    Write-Host "  Created dummy certificate files for testing" -ForegroundColor Gray
    Write-Host "  Note: For production, use proper certificates!" -ForegroundColor Yellow
} else {
    Write-Host "`nGenerating self-signed certificates with OpenSSL..." -ForegroundColor Yellow
    
    $certPath = "blockchain\crypto-config\peerOrganizations\org1.example.com\peers\peer0.org1.example.com\msp\signcerts\peer0.org1.example.com-cert.pem"
    $keyPath = "blockchain\crypto-config\peerOrganizations\org1.example.com\peers\peer0.org1.example.com\msp\keystore\priv_sk"
    
    # Generate self-signed certificate
    $opensslCmd = "req -x509 -newkey rsa:4096 -nodes -keyout `"$keyPath`" -out `"$certPath`" -days 365 -subj `"/C=US/ST=State/L=City/O=Org1/CN=peer0.org1.example.com`""
    
    Start-Process -FilePath "openssl" -ArgumentList $opensslCmd -NoNewWindow -Wait
    Write-Host "  Generated self-signed certificate" -ForegroundColor Gray
}

# Copy certificates
Write-Host "`nCopying certificates..." -ForegroundColor Yellow
$sourceCert = "blockchain\crypto-config\peerOrganizations\org1.example.com\peers\peer0.org1.example.com\msp\signcerts\peer0.org1.example.com-cert.pem"

# Copy as CA cert
Copy-Item -Path $sourceCert -Destination "blockchain\crypto-config\peerOrganizations\org1.example.com\peers\peer0.org1.example.com\msp\cacerts\ca.org1.example.com-cert.pem" -Force
Write-Host "  Copied CA certificate" -ForegroundColor Gray

# Copy as admin cert
Copy-Item -Path $sourceCert -Destination "blockchain\crypto-config\peerOrganizations\org1.example.com\peers\peer0.org1.example.com\msp\admincerts\Admin@org1.example.com-cert.pem" -Force
Write-Host "  Copied Admin certificate" -ForegroundColor Gray

# Save the core.yaml file if it doesn't exist
if (-not (Test-Path "blockchain\config\core.yaml")) {
    Write-Host "`nCreating core.yaml..." -ForegroundColor Yellow
    Write-Host "Please copy the core.yaml content from the previous artifact to: blockchain\config\core.yaml" -ForegroundColor Cyan
}

# Display directory structure
Write-Host "`nDirectory structure created:" -ForegroundColor Green
Get-ChildItem -Path "blockchain" -Recurse -File | Select-Object -First 20 | ForEach-Object {
    Write-Host "  $($_.FullName)" -ForegroundColor Gray
}

Write-Host "`nSetup complete!" -ForegroundColor Green
Write-Host "`nNext steps:" -ForegroundColor Yellow
Write-Host "1. Ensure core.yaml is saved in blockchain\config\" -ForegroundColor White
Write-Host "2. Run: docker-compose down" -ForegroundColor White
Write-Host "3. Run: docker-compose up -d" -ForegroundColor White