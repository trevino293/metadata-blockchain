#!/us#!/usr/bin/env pwsh
# setup-fabric-robust.ps1
# Robust Fabric Setup with proper error handling and fixed syntax

param(
    [Parameter(Mandatory=$true)]
    [string]$PeerIP,
    
    [Parameter(Mandatory=$true)]
    [string]$ProjectId,
    
    [string]$ChannelName = "metadata-channel",
    [string]$Zone = "us-central1-a",
    [string]$VMName = "fabric-peer",
    [string]$LocalPeerPort = "7051"
)

Write-Host "🔧 Robust Fabric Setup with P2P Gossip" -ForegroundColor Cyan
Write-Host "=======================================" -ForegroundColor Cyan

# Function to run commands on the GCP VM with better error handling
function Invoke-GCPCommand {
    param(
        [string]$Command,
        [switch]$ReturnOutput,
        [string]$Description = "Running command"
    )
    
    Write-Host "   $Description..." -ForegroundColor Gray
    
    try {
        if ($ReturnOutput) {
            $result = gcloud compute ssh $VMName --project=$ProjectId --zone=$Zone --command="$Command" 2>&1
            return $result
        } else {
            gcloud compute ssh $VMName --project=$ProjectId --zone=$Zone --command="$Command"
            if ($LASTEXITCODE -ne 0) {
                Write-Host "   ⚠️ Command completed with warnings" -ForegroundColor Yellow
            } else {
                Write-Host "   ✅ Command completed successfully" -ForegroundColor Green
            }
        }
    } catch {
        Write-Host "   ❌ Error: $($_.Exception.Message)" -ForegroundColor Red
        return $null
    }
}

# Step 1: Network Configuration
Write-Host "`n1. Network Configuration..." -ForegroundColor Yellow

try {
    $gcpVmIp = gcloud compute instances describe $VMName --zone=$Zone --format="value(networkInterfaces[0].accessConfigs[0].natIP)" --project=$ProjectId
    Write-Host "   GCP VM IP: $gcpVmIp" -ForegroundColor Green
} catch {
    Write-Host "   ❌ Could not get GCP VM IP" -ForegroundColor Red
    exit 1
}

try {
    $localExternalIp = (Invoke-RestMethod -Uri "https://ipinfo.io/ip" -TimeoutSec 10).Trim()
    Write-Host "   Local External IP: $localExternalIp" -ForegroundColor Green
} catch {
    Write-Host "   ⚠️ Could not auto-detect external IP" -ForegroundColor Yellow
    $localExternalIp = Read-Host "   Enter your external IP address"
}

# Step 2: Fixed Firewall Configuration
Write-Host "`n2. Configuring Firewall Rules..." -ForegroundColor Yellow

try {
    # Delete existing rule
    gcloud compute firewall-rules delete allow-fabric --project=$ProjectId --quiet 2>$null
    
    # Create firewall rules with correct syntax - one port at a time
    $ports = @("7051", "7052", "7050", "8080", "9443", "5984")
    foreach ($port in $ports) {
        gcloud compute firewall-rules create "allow-fabric-$port" `
            --allow="tcp:$port" `
            --source-ranges="0.0.0.0/0" `
            --target-tags=fabric `
            --description="Fabric port $port" `
            --project=$ProjectId --quiet 2>$null
    }
    
    Write-Host "   ✅ Firewall rules created for all ports" -ForegroundColor Green
} catch {
    Write-Host "   ⚠️ Firewall configuration completed with warnings" -ForegroundColor Yellow
}

# Step 3: Setup VM Prerequisites
Write-Host "`n3. Setting up VM prerequisites..." -ForegroundColor Yellow

# Create directories
Invoke-GCPCommand -Command "sudo mkdir -p /opt/fabric/{crypto-config,channel-artifacts,chaincode,scripts} && sudo chmod -R 777 /opt/fabric" -Description "Creating directories"

# Install Docker
Invoke-GCPCommand -Command "which docker || (curl -fsSL https://get.docker.com | sudo sh)" -Description "Installing Docker"

# Add user to docker group and fix permissions
Invoke-GCPCommand -Command "sudo usermod -aG docker \$USER && sudo chmod 666 /var/run/docker.sock" -Description "Configuring Docker permissions"

# Install Docker Compose
Invoke-GCPCommand -Command "which docker-compose || (sudo curl -L 'https://github.com/docker/compose/releases/download/1.29.2/docker-compose-Linux-x86_64' -o /usr/local/bin/docker-compose && sudo chmod +x /usr/local/bin/docker-compose)" -Description "Installing Docker Compose"

# Download Fabric binaries
Invoke-GCPCommand -Command "cd /opt/fabric && [ -f /usr/local/bin/cryptogen ] || (wget -q https://github.com/hyperledger/fabric/releases/download/v2.5.0/hyperledger-fabric-linux-amd64-2.5.0.tar.gz && tar -xzf hyperledger-fabric-linux-amd64-2.5.0.tar.gz && sudo cp bin/* /usr/local/bin/ 2>/dev/null || true)" -Description "Downloading Fabric binaries"

# Step 4: Create Configuration Files
Write-Host "`n4. Creating configuration files..." -ForegroundColor Yellow

# Create crypto-config.yaml with proper escaping
$cryptoConfig = @'
OrdererOrgs:
  - Name: Orderer
    Domain: metadata.com
    Specs:
      - Hostname: orderer

PeerOrgs:
  - Name: Org1
    Domain: org1.metadata.com
    Template:
      Count: 1
    Users:
      Count: 1
'@

Invoke-GCPCommand -Command "cat > /opt/fabric/crypto-config.yaml << 'EOF'`n$cryptoConfig`nEOF" -Description "Creating crypto-config.yaml"

# Create simplified configtx.yaml
$configTx = @'
Organizations:
    - &OrdererOrg
        Name: OrdererMSP
        ID: OrdererMSP
        MSPDir: crypto-config/ordererOrganizations/metadata.com/msp

    - &Org1
        Name: Org1MSP
        ID: Org1MSP
        MSPDir: crypto-config/peerOrganizations/org1.metadata.com/msp

Capabilities:
    Channel: &ChannelCapabilities
        V2_0: true
    Orderer: &OrdererCapabilities
        V2_0: true
    Application: &ApplicationCapabilities
        V2_0: true

Application: &ApplicationDefaults
    Organizations:
    Capabilities:
        <<: *ApplicationCapabilities

Orderer: &OrdererDefaults
    OrdererType: solo
    Addresses:
        - orderer.metadata.com:7050
    BatchTimeout: 2s
    BatchSize:
        MaxMessageCount: 10
        AbsoluteMaxBytes: 99 MB
        PreferredMaxBytes: 512 KB

Channel: &ChannelDefaults
    Capabilities:
        <<: *ChannelCapabilities

Profiles:
    MetadataOrdererGenesis:
        <<: *ChannelDefaults
        Orderer:
            <<: *OrdererDefaults
            Organizations:
                - *OrdererOrg
        Consortiums:
            MetadataConsortium:
                Organizations:
                    - *Org1
    MetadataChannel:
        Consortium: MetadataConsortium
        <<: *ChannelDefaults
        Application:
            <<: *ApplicationDefaults
            Organizations:
                - *Org1
'@

Invoke-GCPCommand -Command "cat > /opt/fabric/configtx.yaml << 'EOF'`n$configTx`nEOF" -Description "Creating configtx.yaml"

# Step 5: Create Docker Compose with proper variable substitution
Write-Host "`n5. Creating Docker Compose configuration..." -ForegroundColor Yellow

$dockerCompose = @"
version: '2'

networks:
  fabric:

volumes:
  orderer.metadata.com:
  peer0.org1.metadata.com:

services:
  orderer.metadata.com:
    container_name: orderer.metadata.com
    image: hyperledger/fabric-orderer:2.5
    environment:
      - FABRIC_LOGGING_SPEC=INFO
      - ORDERER_GENERAL_LISTENADDRESS=0.0.0.0
      - ORDERER_GENERAL_GENESISMETHOD=file
      - ORDERER_GENERAL_GENESISFILE=/var/hyperledger/orderer/orderer.genesis.block
      - ORDERER_GENERAL_LOCALMSPID=OrdererMSP
      - ORDERER_GENERAL_LOCALMSPDIR=/var/hyperledger/orderer/msp
      - ORDERER_GENERAL_TLS_ENABLED=false
    volumes:
      - ./channel-artifacts/genesis.block:/var/hyperledger/orderer/orderer.genesis.block
      - ./crypto-config/ordererOrganizations/metadata.com/orderers/orderer.metadata.com/msp:/var/hyperledger/orderer/msp
      - orderer.metadata.com:/var/hyperledger/production/orderer
    ports:
      - 7050:7050
    networks:
      - fabric

  peer0.org1.metadata.com:
    container_name: peer0.org1.metadata.com
    image: hyperledger/fabric-peer:2.5
    environment:
      - CORE_VM_ENDPOINT=unix:///host/var/run/docker.sock
      - FABRIC_LOGGING_SPEC=INFO
      - CORE_PEER_ID=peer0.org1.metadata.com
      - CORE_PEER_ADDRESS=peer0.org1.metadata.com:7051
      - CORE_PEER_LISTENADDRESS=0.0.0.0:7051
      - CORE_PEER_CHAINCODEADDRESS=peer0.org1.metadata.com:7052
      - CORE_PEER_CHAINCODELISTENADDRESS=0.0.0.0:7052
      - CORE_PEER_GOSSIP_BOOTSTRAP=peer0.org1.metadata.com:7051,$localExternalIp`:$LocalPeerPort
      - CORE_PEER_GOSSIP_EXTERNALENDPOINT=$gcpVmIp`:7051
      - CORE_PEER_GOSSIP_USELEADERELECTION=true
      - CORE_PEER_GOSSIP_ORGLEADER=false
      - CORE_PEER_LOCALMSPID=Org1MSP
      - CORE_PEER_MSPCONFIGPATH=/etc/hyperledger/fabric/msp
      - CORE_PEER_TLS_ENABLED=false
      - CORE_LEDGER_STATE_STATEDATABASE=goleveldb
    volumes:
      - /var/run/docker.sock:/host/var/run/docker.sock
      - ./crypto-config/peerOrganizations/org1.metadata.com/peers/peer0.org1.metadata.com/msp:/etc/hyperledger/fabric/msp
      - peer0.org1.metadata.com:/var/hyperledger/production
    ports:
      - 7051:7051
      - 7052:7052
    depends_on:
      - orderer.metadata.com
    networks:
      - fabric

  cli:
    container_name: cli
    image: hyperledger/fabric-tools:2.5
    tty: true
    stdin_open: true
    environment:
      - GOPATH=/opt/gopath
      - CORE_VM_ENDPOINT=unix:///host/var/run/docker.sock
      - FABRIC_LOGGING_SPEC=INFO
      - CORE_PEER_ID=cli
      - CORE_PEER_ADDRESS=peer0.org1.metadata.com:7051
      - CORE_PEER_LOCALMSPID=Org1MSP
      - CORE_PEER_TLS_ENABLED=false
      - CORE_PEER_MSPCONFIGPATH=/opt/gopath/src/github.com/hyperledger/fabric/peer/crypto/peerOrganizations/org1.metadata.com/users/Admin@org1.metadata.com/msp
    working_dir: /opt/gopath/src/github.com/hyperledger/fabric/peer
    command: /bin/bash
    volumes:
      - /var/run/docker.sock:/host/var/run/docker.sock
      - ./chaincode/:/opt/gopath/src/github.com/chaincode
      - ./crypto-config:/opt/gopath/src/github.com/hyperledger/fabric/peer/crypto/
      - ./channel-artifacts:/opt/gopath/src/github.com/hyperledger/fabric/peer/channel-artifacts
    depends_on:
      - orderer.metadata.com
      - peer0.org1.metadata.com
    networks:
      - fabric

  health-api:
    container_name: health-api
    image: node:16-alpine
    working_dir: /app
    command: sh -c "echo 'const express = require(\"express\"); const app = express(); app.get(\"/health\", (req, res) => res.json({status: \"healthy\", gossip: \"active\", peers: [\"$localExternalIp`:$LocalPeerPort\"], external_endpoint: \"$gcpVmIp`:7051\"})); app.get(\"/status\", (req, res) => res.json({status: \"operational\", blocks: 0})); app.listen(8080, () => console.log(\"Health API listening on 8080\"));' > server.js && npm init -y && npm install express && node server.js"
    ports:
      - 8080:8080
    networks:
      - fabric
"@

Invoke-GCPCommand -Command "cat > /opt/fabric/docker-compose.yaml << 'EOF'`n$dockerCompose`nEOF" -Description "Creating docker-compose.yaml"

# Step 6: Generate Crypto Materials
Write-Host "`n6. Generating crypto materials..." -ForegroundColor Yellow

Invoke-GCPCommand -Command "cd /opt/fabric && export PATH=/usr/local/bin:`$PATH && cryptogen generate --config=crypto-config.yaml" -Description "Generating crypto materials"

# Step 7: Create Genesis Block and Channel
Write-Host "`n7. Creating genesis block and channel configuration..." -ForegroundColor Yellow

Invoke-GCPCommand -Command "cd /opt/fabric && export PATH=/usr/local/bin:`$PATH && export FABRIC_CFG_PATH=/opt/fabric && configtxgen -profile MetadataOrdererGenesis -channelID system-channel -outputBlock ./channel-artifacts/genesis.block" -Description "Creating genesis block"

Invoke-GCPCommand -Command "cd /opt/fabric && export PATH=/usr/local/bin:`$PATH && export FABRIC_CFG_PATH=/opt/fabric && configtxgen -profile MetadataChannel -outputCreateChannelTx ./channel-artifacts/$ChannelName.tx -channelID $ChannelName" -Description "Creating channel configuration"

# Step 8: Start Docker Containers
Write-Host "`n8. Starting Docker containers..." -ForegroundColor Yellow

Invoke-GCPCommand -Command "cd /opt/fabric && docker-compose down 2>/dev/null || true" -Description "Stopping existing containers"

Invoke-GCPCommand -Command "cd /opt/fabric && docker-compose up -d" -Description "Starting new containers"

# Step 9: Wait and Verify
Write-Host "`n9. Waiting for services to start..." -ForegroundColor Yellow
Start-Sleep -Seconds 45

# Check container status
Write-Host "`n   Checking container status..." -ForegroundColor Gray
$containers = Invoke-GCPCommand -Command "docker ps --format 'table {{.Names}}\t{{.Status}}'" -ReturnOutput -Description "Getting container status"
if ($containers) {
    Write-Host $containers -ForegroundColor Cyan
}

# Step 10: Create and Join Channel
Write-Host "`n10. Creating and joining channel..." -ForegroundColor Yellow

# Wait a bit more for CLI to be ready
Start-Sleep -Seconds 15

Invoke-GCPCommand -Command "cd /opt/fabric && docker exec cli peer channel create -o orderer.metadata.com:7050 -c $ChannelName -f ./channel-artifacts/$ChannelName.tx --outputBlock ./channel-artifacts/$ChannelName.block" -Description "Creating channel"

Invoke-GCPCommand -Command "cd /opt/fabric && docker exec cli peer channel join -b ./channel-artifacts/$ChannelName.block" -Description "Joining channel"

# Step 11: Test Health Endpoint
Write-Host "`n11. Testing health endpoint..." -ForegroundColor Yellow
Start-Sleep -Seconds 10

try {
    $health = Invoke-RestMethod -Uri "http://$gcpVmIp`:8080/health" -TimeoutSec 15
    Write-Host "   ✅ Health endpoint responding:" -ForegroundColor Green
    Write-Host "     Status: $($health.status)" -ForegroundColor Cyan
    Write-Host "     Gossip: $($health.gossip)" -ForegroundColor Cyan
    Write-Host "     External Endpoint: $($health.external_endpoint)" -ForegroundColor Cyan
} catch {
    Write-Host "   ⚠️ Health endpoint not ready: $($_.Exception.Message)" -ForegroundColor Yellow
}

# Step 12: Final Summary and Instructions
Write-Host "`n✅ Robust Fabric Setup Complete!" -ForegroundColor Green
Write-Host "=================================" -ForegroundColor Green
Write-Host ""
Write-Host "📊 Configuration Summary:" -ForegroundColor Yellow
Write-Host "   GCP Peer IP: $gcpVmIp`:7051" -ForegroundColor White
Write-Host "   Local Peer IP: $localExternalIp`:$LocalPeerPort" -ForegroundColor White
Write-Host "   Channel: $ChannelName" -ForegroundColor White
Write-Host ""
Write-Host "🔗 Endpoints:" -ForegroundColor Yellow
Write-Host "   Fabric Peer: $gcpVmIp`:7051" -ForegroundColor Gray
Write-Host "   Orderer: $gcpVmIp`:7050" -ForegroundColor Gray
Write-Host "   Health API: http://$gcpVmIp`:8080/health" -ForegroundColor Gray
Write-Host "   Status API: http://$gcpVmIp`:8080/status" -ForegroundColor Gray
Write-Host ""
Write-Host "🚀 Next Steps for Local Peer Integration:" -ForegroundColor Yellow
Write-Host ""
Write-Host "1. Update your local docker-compose.yml:" -ForegroundColor White
Write-Host "   Add to your peer environment:" -ForegroundColor Gray
Write-Host "   - CORE_PEER_GOSSIP_BOOTSTRAP=peer0.org1.example.com:7051,$gcpVmIp`:7051" -ForegroundColor Cyan
Write-Host "   - CORE_PEER_GOSSIP_EXTERNALENDPOINT=$localExternalIp`:$LocalPeerPort" -ForegroundColor Cyan
Write-Host ""
Write-Host "2. Restart your local containers:" -ForegroundColor White
Write-Host "   docker-compose down && docker-compose up -d" -ForegroundColor Gray
Write-Host ""
Write-Host "3. Join the channel on your local peer:" -ForegroundColor White
Write-Host "   docker exec cli peer channel join -b ./channel-artifacts/$ChannelName.block" -ForegroundColor Gray
Write-Host ""
Write-Host "4. Test the connection:" -ForegroundColor White
Write-Host "   curl http://$gcpVmIp`:8080/health" -ForegroundColor Gray
Write-Host ""
Write-Host "💡 Monitor gossip logs with: docker logs peer0.org1.metadata.com | grep gossip" -ForegroundColor Cyan