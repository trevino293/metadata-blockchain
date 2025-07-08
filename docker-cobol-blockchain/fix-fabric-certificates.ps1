# fix-fabric-certificates.ps1 - Fix certificate directory structure

Write-Host "Fixing Hyperledger Fabric Certificate Structure" -ForegroundColor Cyan
Write-Host "==============================================" -ForegroundColor Cyan

# Check current directory structure
Write-Host "`nChecking current directory structure..." -ForegroundColor Yellow
$mspPath = "blockchain\crypto-config\peerOrganizations\org1.example.com\peers\peer0.org1.example.com\msp"

# Create all required directories
$directories = @(
    "$mspPath",
    "$mspPath\admincerts",
    "$mspPath\cacerts",
    "$mspPath\keystore",
    "$mspPath\signcerts",
    "$mspPath\tlscacerts"
)

foreach ($dir in $directories) {
    if (Test-Path $dir) {
        Write-Host "✓ Directory exists: $dir" -ForegroundColor Green
    } else {
        New-Item -ItemType Directory -Force -Path $dir | Out-Null
        Write-Host "✓ Created directory: $dir" -ForegroundColor Yellow
    }
}

# Create test certificates if they don't exist
Write-Host "`nCreating test certificates..." -ForegroundColor Yellow

# Create a simple test certificate content
$testCertificate = @"
-----BEGIN CERTIFICATE-----
MIIDXTCCAkWgAwIBAgIJAKl1GZcW4jLFMA0GCSqGSIb3DQEBCwUAMEUxCzAJBgNV
BAYTAlVTMRMwEQYDVQQIDApTb21lLVN0YXRlMSEwHwYDVQQKDBhJbnRlcm5ldCBX
aWRnaXRzIFB0eSBMdGQwHhcNMjQwNjIzMDAwMDAwWhcNMjUwNjIzMDAwMDAwWjBF
MQswCQYDVQQGEwJVUzETMBEGA1UECAwKU29tZS1TdGF0ZTEhMB8GA1UECgwYSW50
ZXJuZXQgV2lkZ2l0cyBQdHkgTHRkMIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIB
CgKCAQEAx3M3kQMbVJu2YdJh5gFpGcW1oZAI7RAPrCwQ+VbhNPdXKRvea4P2Xoor
A7fd2FM5tb9EAW8dvC9p3LvZ/L1nxQGqM7nHRKUlXCgsxBB6f8ML5IKcYQNBN6Ho
HgkhmPm1RmXq2Q2GdK/xDKFUBgo1FHKVdRW0OLHqT+F4s3D3L1ATBvV7d6nfJKu8
GcwIHEgqQvQvmQHel9/UeFbP8v5V5JV6DE9dVbC8REbT1bTsXNhVXCCQYME3n1kX
vWID1/P1p1qfpHL8nEadfFJhe8Uo5kDbrN3t5rmAna6SZog1GgMvUWYH5TDWdBvF
P0xLJxVnBFsBV7lXrwHLnIhGmQvL5wIDAQABo1AwTjAdBgNVHQ4EFgQUzKVXH9WL
qgvURJqbXKP1S1c5MwMwHwYDVR0jBBgwFoAUzKVXH9WLqgvURJqbXKP1S1c5MwMw
DAYDVR0TBAUwAwEB/zANBgkqhkiG9w0BAQsFAAOCAQEAo7Y9k6kYsYJRKZF9gWq5
CaZkKNmSVb8e3J6W7C2G3lDKz4R8MhfVmTgLkP3a5mkfqH6TGzN2Tc8yMPWJQL3w
+kUWUkr8diCmJFwraBRzC2VW2HBKA9RM1TBVMR7VvZbKz4YT1sqDQvjHSO8492AH
P+nazQFmSRGJw3H2eCqdI5tMmNLIXuFSqKcHKaQvq0L5ScE1hGR5qfNHcJICYcHm
OakMLhJAs3tIUcAVXHRDb1GaVYCDX3wSBzZ7eI1n9DE3VzQeNjJYvt7bOenozQ1A
VvYjBe1J9bqrVPHvvwoGOGl5WTksL6s2S7Q8BZ6xVhzqFMyUGBQGrgBp5gkLvZ0c
Rg==
-----END CERTIFICATE-----
"@

$testPrivateKey = @"
-----BEGIN PRIVATE KEY-----
MIIEvgIBADANBgkqhkiG9w0BAQEFAASCBKgwggSkAgEAAoIBAQDHczeRAxuUm7Zh
0mHmAWkZxbWhkAjtEA+sLBD5VuE091cpG95rg/ZeiisD593YUzm1v0QBbx28L2nc
u9n8vWfFAaozucdEpSVcKCzEEHp/wwvkgpxhA0E3oegeCGSY+bVGZerZDYZ0r/EM
oVQGCjUUcpV1FbQ4sepP4XizcPcvUBMG9Xt3qd8kq7wZzAgcSCpC9C+ZAd6X39R4
Vs/y/lXklXoMT11VsLxERtPVtOxc2FVcIJBgwTefWRe9YgPX8/WnWp+kcvycRp18
UmF7xSjmQNus3e3muYCdrpJmiDUaAy9RZgflMNZ0G8U/TEsnFWcEWwFXuVevAcuc
iEaZC8vnAgMBAAECggEBAIzNP9iHsLN6KlLcGplYeJWz2YA3FdYQX+L7mAVzwVId
xrR0ITm9UvdkJ5wTiQuvvWr7/mJZFw/RdLoAK1yhbUhB1PLrqH+poZ2X7myvU5LG
7ccTNZUUPKqP3xJHYHDVVBisZQD7A1Z2x7MZz7LqH8ZGCL0V72koNKSQO3KjXALL
VQQKoYPoqpEcRQ0tltQVD8Y/QZpkdy2rJgYa8M3Y2WGBq2N9D7dHB6aL7B3dWf/X
kY8qcD8SfpSSHEPFK7fPKdVXwJLIt1Y0mJz7Y2gvn0dLnqPJKPr+22Xl8mefjNO4
5aCmQ8y4VJOxl7q3PnuFNFpOhYX+7uhlHJ+Q+S5irkECgYEA5eLwX2cFwvheC6f+
+ztMxBw7qQT+FCcMxOKoreTPNEPp/wXLfVpDDQhg3KrsS9K3pJKyugaGJjZ0qt5m
sW9RJNmUYGB4w48cJWInOR2in5h0d0K3jNF7KpQxMLHQFBYxIX6TJJQNhbprgQKf
1E7SaiFPgmholH8e1Y0e4LxVaccCgYEA3p8UPy4uwDD9dfFOYxGhKjwnRiT4kdxW
d7c4rVJvQVJwJ1RqsGOFBjI/2EfHSSBob8Y8PkYFJicUkn8YPxmx3awcG6hhA0sI
2c9zqQxva2fOUMbkB1tZ5KQr1L7F3xdZHTb7V5HdDKHx+GmwcOkxwc1Q1+W2jKPu
5JhOSjw4QEECgYEAo+s9dP6Y5SHJQV8vFaXB8aMhRKiLU5F8YLou5Mv6OXzbGGAB
Ku9qImSMldCUp7oh5TI5WASj/6exMJU2/HH7ydo8RqfDQ2ECrQqLzJXbyX2F3JA5
jgnTBwRcBJnhB2hHFvqh2f3DE3WdN2WVO4uDCG3GrLAz4kQ8LOoJSB1F5AsCgYBm
G6rFR7JQIT7IlhLXMFj3wpE/P0cuPKR2uBMPVnILlBKJD3fFDW0x6E3gQUNPda3r
4urDE6mCQ3XMX1RKYXn+vgKnRjoL6uLlSKJDXcRuN3CPHaFOYAQdBDNaqjPQYQA5
r0U/1HN4oBd8hZXGvMQ7Xvx1MlwvJY7uVQY4SEqBAQKBgFgYv5w6l4UB7VhXHREa
6OgH7ePXw3JIucqRCF7yr2CjQdLDQF2OXxRQ8Y3Y3gBegJ5KwV2SH3ToszPJFGu6
W6D3nEHlACmYojQokPgfaP2JdD7KcD7nXjMTWTVV8yH0B1BzQJL4lQ6MFJ5qkPa4
UKfYI0nc6NdpQJVL+gS9yJ0L
-----END PRIVATE KEY-----
"@

# Save certificates
$certFiles = @{
    "$mspPath\signcerts\peer0.org1.example.com-cert.pem" = $testCertificate
    "$mspPath\cacerts\ca.org1.example.com-cert.pem" = $testCertificate
    "$mspPath\admincerts\Admin@org1.example.com-cert.pem" = $testCertificate
    "$mspPath\keystore\priv_sk" = $testPrivateKey
}

foreach ($file in $certFiles.Keys) {
    $certFiles[$file] | Out-File -FilePath $file -Encoding ASCII -NoNewline
    Write-Host "✓ Created: $file" -ForegroundColor Green
}

# Create MSP config.yaml
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

$mspConfig | Out-File -FilePath "$mspPath\config.yaml" -Encoding ASCII -NoNewline
Write-Host "✓ Created: $mspPath\config.yaml" -ForegroundColor Green

# Verify file structure
Write-Host "`nVerifying MSP structure:" -ForegroundColor Yellow
Get-ChildItem -Path $mspPath -Recurse | Where-Object { -not $_.PSIsContainer } | ForEach-Object {
    $relativePath = $_.FullName.Replace("$PWD\", "")
    Write-Host "  $relativePath" -ForegroundColor Gray
}

# Show docker volume mount command
Write-Host "`nDocker volume mount verification:" -ForegroundColor Yellow
$absolutePath = (Resolve-Path $mspPath).Path
Write-Host "Local path: $absolutePath" -ForegroundColor Cyan
Write-Host "This should be mounted to: /etc/hyperledger/fabric/msp" -ForegroundColor Cyan

Write-Host "`nNext steps:" -ForegroundColor Yellow
Write-Host "1. Stop any running containers: docker-compose down" -ForegroundColor White
Write-Host "2. Remove volumes: docker volume prune" -ForegroundColor White
Write-Host "3. Start fresh: docker-compose up -d" -ForegroundColor White
Write-Host "4. Check logs: docker-compose logs hyperledger-peer" -ForegroundColor White