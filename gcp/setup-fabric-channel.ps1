# setup-fabric-direct.ps1
# Direct execution approach - no file creation issues

param(
    [Parameter(Mandatory=$true)]
    [string]$PeerIP,
    
    [Parameter(Mandatory=$true)]
    [string]$ProjectId,
    
    [string]$ChannelName = "metadata-channel",
    [string]$Zone = "us-central1-a",
    [string]$VMName = "fabric-peer"
)

Write-Host "🔧 Setting up Fabric Channel: $ChannelName" -ForegroundColor Cyan
Write-Host "=====================================" -ForegroundColor Cyan
Write-Host ""

# Function to run commands on the GCP VM
function Invoke-GCPCommand {
    param(
        [string]$Command,
        [switch]$ReturnOutput
    )
    
    $sshCommand = "gcloud compute ssh $VMName --project=$ProjectId --zone=$Zone --command=`"$Command`" 2>&1"
    
    if ($ReturnOutput) {
        $output = Invoke-Expression $sshCommand
        return $output
    } else {
        Invoke-Expression $sshCommand
    }
}

Write-Host "1. Checking VM connectivity..." -ForegroundColor Yellow
$vmStatus = gcloud compute instances describe $VMName --project=$ProjectId --zone=$Zone --format="value(status)" 2>$null

if ($vmStatus -ne "RUNNING") {
    Write-Host "   ❌ VM is not running. Current status: $vmStatus" -ForegroundColor Red
    exit 1
}

Write-Host "   ✅ VM is running" -ForegroundColor Green

# Execute setup directly via SSH commands
Write-Host ""
Write-Host "2. Setting up directories..." -ForegroundColor Yellow
Invoke-GCPCommand -Command "sudo mkdir -p /opt/fabric/{crypto-config,channel-artifacts,chaincode,pubsub-adapter,scripts} && sudo chmod -R 777 /opt/fabric"

Write-Host ""
Write-Host "3. Installing prerequisites..." -ForegroundColor Yellow

# Install Docker
Write-Host "   Installing Docker..." -ForegroundColor Gray
Invoke-GCPCommand -Command "which docker || (curl -fsSL https://get.docker.com | sudo sh && sudo usermod -aG docker $USER)"

# Install Docker Compose
Write-Host "   Installing Docker Compose..." -ForegroundColor Gray
Invoke-GCPCommand -Command "which docker-compose || (sudo curl -L 'https://github.com/docker/compose/releases/download/1.29.2/docker-compose-Linux-x86_64' -o /usr/local/bin/docker-compose && sudo chmod +x /usr/local/bin/docker-compose)"

Write-Host ""
Write-Host "4. Downloading Fabric binaries..." -ForegroundColor Yellow
Invoke-GCPCommand -Command "cd /opt/fabric && [ -f /usr/local/bin/cryptogen ] || (wget -q https://github.com/hyperledger/fabric/releases/download/v2.5.0/hyperledger-fabric-linux-amd64-2.5.0.tar.gz && tar -xzf hyperledger-fabric-linux-amd64-2.5.0.tar.gz && sudo cp bin/* /usr/local/bin/ && rm -rf bin config hyperledger-fabric-linux-amd64-2.5.0.tar.gz)"

Write-Host ""
Write-Host "5. Creating configuration files..." -ForegroundColor Yellow

# Create crypto-config.yaml using echo commands
Write-Host "   Creating crypto-config.yaml..." -ForegroundColor Gray
$cryptoConfig = @"
echo 'OrdererOrgs:
  - Name: Orderer
    Domain: metadata.com
    EnableNodeOUs: true
    Specs:
      - Hostname: orderer

PeerOrgs:
  - Name: Org1
    Domain: org1.metadata.com
    EnableNodeOUs: true
    Template:
      Count: 1
    Users:
      Count: 1' > /opt/fabric/crypto-config.yaml
"@
Invoke-GCPCommand -Command $cryptoConfig

# Create configtx.yaml
Write-Host "   Creating configtx.yaml..." -ForegroundColor Gray
Invoke-GCPCommand -Command "cd /opt/fabric && cat > configtx.yaml << 'CONFIGEOF'
Organizations:
    - &OrdererOrg
        Name: OrdererOrg
        ID: OrdererMSP
        MSPDir: crypto-config/ordererOrganizations/metadata.com/msp

    - &Org1
        Name: Org1MSP
        ID: Org1MSP
        MSPDir: crypto-config/peerOrganizations/org1.metadata.com/msp
        AnchorPeers:
            - Host: localhost
              Port: 7051

Capabilities:
    Channel: &ChannelCapabilities
        V2_0: true
    Orderer: &OrdererCapabilities
        V2_0: true
    Application: &ApplicationCapabilities
        V2_0: true

Application: &ApplicationDefaults
    Organizations:

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
    Policies:
        Readers:
            Type: ImplicitMeta
            Rule: \"ANY Readers\"
        Writers:
            Type: ImplicitMeta
            Rule: \"ANY Writers\"
        Admins:
            Type: ImplicitMeta
            Rule: \"MAJORITY Admins\"

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
CONFIGEOF"

# Create docker-compose.yaml
Write-Host "   Creating docker-compose.yaml..." -ForegroundColor Gray
Invoke-GCPCommand -Command "cd /opt/fabric && cat > docker-compose.yaml << 'DOCKEREOF'
version: '2'

networks:
  fabric:

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
    ports:
      - 7050:7050
    networks:
      - fabric

  peer0.org1.metadata.com:
    container_name: peer0.org1.metadata.com
    image: hyperledger/fabric-peer:2.5
    environment:
      - CORE_VM_ENDPOINT=unix:///host/var/run/docker.sock
      - CORE_VM_DOCKER_HOSTCONFIG_NETWORKMODE=fabric_fabric
      - CORE_PEER_ID=peer0.org1.metadata.com
      - CORE_PEER_ADDRESS=peer0.org1.metadata.com:7051
      - CORE_PEER_LISTENADDRESS=0.0.0.0:7051
      - CORE_PEER_CHAINCODEADDRESS=peer0.org1.metadata.com:7052
      - CORE_PEER_CHAINCODELISTENADDRESS=0.0.0.0:7052
      - CORE_PEER_GOSSIP_BOOTSTRAP=peer0.org1.metadata.com:7051
      - CORE_PEER_GOSSIP_EXTERNALENDPOINT=peer0.org1.metadata.com:7051
      - CORE_PEER_LOCALMSPID=Org1MSP
      - FABRIC_LOGGING_SPEC=INFO
      - CORE_PEER_TLS_ENABLED=false
    volumes:
      - /var/run/:/host/var/run/
      - ./crypto-config/peerOrganizations/org1.metadata.com/peers/peer0.org1.metadata.com/msp:/etc/hyperledger/fabric/msp
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
      - /var/run/:/host/var/run/
      - ./chaincode/:/opt/gopath/src/github.com/chaincode
      - ./crypto-config:/opt/gopath/src/github.com/hyperledger/fabric/peer/crypto/
      - ./channel-artifacts:/opt/gopath/src/github.com/hyperledger/fabric/peer/channel-artifacts
    depends_on:
      - peer0.org1.metadata.com
    networks:
      - fabric

  pubsub-adapter:
    container_name: pubsub-adapter
    image: node:14-alpine
    environment:
      - PEER_ADDRESS=peer0.org1.metadata.com:7051
      - PROJECT_ID=$ProjectId
      - SUBSCRIPTION_NAME=fabric-ingestion
    volumes:
      - ./pubsub-adapter:/app
    working_dir: /app
    command: sh -c \"apk add --no-cache python3 make g++ && npm install && node server.js\"
    ports:
      - 8080:8080
    depends_on:
      - peer0.org1.metadata.com
    networks:
      - fabric
DOCKEREOF"

Write-Host ""
Write-Host "6. Creating Pub/Sub adapter..." -ForegroundColor Yellow
Invoke-GCPCommand -Command "mkdir -p /opt/fabric/pubsub-adapter"

# Create package.json
Invoke-GCPCommand -Command "cat > /opt/fabric/pubsub-adapter/package.json << 'PKGEOF'
{
  \"name\": \"fabric-pubsub-adapter\",
  \"version\": \"1.0.0\",
  \"dependencies\": {
    \"@google-cloud/pubsub\": \"^3.0.0\",
    \"express\": \"^4.18.0\"
  }
}
PKGEOF"

# Create server.js
Invoke-GCPCommand -Command "cat > /opt/fabric/pubsub-adapter/server.js << 'SERVEREOF'
const express = require('express');
const app = express();
app.use(express.json());

const projectId = process.env.PROJECT_ID;
const subscriptionName = process.env.SUBSCRIPTION_NAME;

app.get('/health', (req, res) => {
    res.json({ 
        status: 'healthy',
        pubsub: { project: projectId, subscription: subscriptionName },
        timestamp: new Date().toISOString()
    });
});

app.post('/test-message', (req, res) => {
    console.log('Test message:', req.body);
    res.json({ status: 'received', data: req.body });
});

const PORT = 8080;
app.listen(PORT, () => {
    console.log(\`Adapter listening on port \${PORT}\`);
});
SERVEREOF"

Write-Host ""
Write-Host "7. Generating crypto materials..." -ForegroundColor Yellow
Invoke-GCPCommand -Command "cd /opt/fabric && export PATH=/usr/local/bin:\$PATH && export FABRIC_CFG_PATH=/opt/fabric && cryptogen generate --config=./crypto-config.yaml"

Write-Host ""
Write-Host "8. Creating genesis block and channel configuration..." -ForegroundColor Yellow
Invoke-GCPCommand -Command "cd /opt/fabric && export PATH=/usr/local/bin:\$PATH && export FABRIC_CFG_PATH=/opt/fabric && configtxgen -profile MetadataOrdererGenesis -channelID system-channel -outputBlock ./channel-artifacts/genesis.block"
Invoke-GCPCommand -Command "cd /opt/fabric && export PATH=/usr/local/bin:\$PATH && export FABRIC_CFG_PATH=/opt/fabric && configtxgen -profile MetadataChannel -outputCreateChannelTx ./channel-artifacts/$ChannelName.tx -channelID $ChannelName"

Write-Host ""
Write-Host "9. Starting Docker containers..." -ForegroundColor Yellow
Invoke-GCPCommand -Command "cd /opt/fabric && sudo docker-compose down 2>/dev/null || true"
Invoke-GCPCommand -Command "cd /opt/fabric && sudo docker-compose up -d"

Write-Host ""
Write-Host "10. Waiting for services to start..." -ForegroundColor Yellow
Start-Sleep -Seconds 20

Write-Host ""
Write-Host "11. Creating and joining channel..." -ForegroundColor Yellow
try {
    # Create channel
    Invoke-GCPCommand -Command "cd /opt/fabric && sudo docker exec cli peer channel create -o orderer.metadata.com:7050 -c $ChannelName -f ./channel-artifacts/$ChannelName.tx --outputBlock ./channel-artifacts/$ChannelName.block"
    
    # Join channel
    Invoke-GCPCommand -Command "cd /opt/fabric && sudo docker exec cli peer channel join -b ./channel-artifacts/$ChannelName.block"
    
    # List channels
    $channels = Invoke-GCPCommand -Command "cd /opt/fabric && sudo docker exec cli peer channel list" -ReturnOutput
    Write-Host "   Channels: $channels" -ForegroundColor Gray
}
catch {
    Write-Host "   ⚠️  Channel operations may need manual intervention" -ForegroundColor Yellow
}

Write-Host ""
Write-Host "12. Verifying deployment..." -ForegroundColor Yellow
$containers = Invoke-GCPCommand -Command "sudo docker ps --format 'table {{.Names}}\t{{.Status}}'" -ReturnOutput
Write-Host $containers -ForegroundColor Gray

Write-Host ""
Write-Host "✅ Fabric setup complete!" -ForegroundColor Green
Write-Host ""
Write-Host "📋 Commands to verify:" -ForegroundColor Cyan
Write-Host "   SSH to VM:" -ForegroundColor White
Write-Host "   gcloud compute ssh $VMName --project=$ProjectId --zone=$Zone" -ForegroundColor Gray
Write-Host ""
Write-Host "   Check containers:" -ForegroundColor White
Write-Host "   sudo docker ps" -ForegroundColor Gray
Write-Host ""
Write-Host "   Check channel:" -ForegroundColor White
Write-Host "   sudo docker exec cli peer channel list" -ForegroundColor Gray
Write-Host ""
Write-Host "🔍 Test endpoints:" -ForegroundColor Yellow
Write-Host "   Health: http://${PeerIP}:8080/health" -ForegroundColor Gray
Write-Host "   Fabric Peer: ${PeerIP}:7051" -ForegroundColor Gray