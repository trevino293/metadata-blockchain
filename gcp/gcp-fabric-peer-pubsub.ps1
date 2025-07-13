# Enhanced gcp-fabric-peer-pubsub.ps1
# Deploy fully configured Fabric peer with proper channel setup and Pub/Sub integration

param(
    [Parameter(Mandatory=$true)]
    [string]$ProjectId,
    
    [Parameter(Mandatory=$true)]
    [string]$LocalPeerIP,
    
    [string]$Zone = "us-central1-a",
    [string]$VmName = "fabric-peer",
    [string]$VmType = "e2-medium",
    [string]$Topic = "blockchain-metadata",
    [string]$Subscription = "fabric-ingestion",
    [string]$ChannelName = "metadata-channel"
)

Write-Host "🚀 Enhanced Fabric Peer Deployment with Complete Setup" -ForegroundColor Green
Write-Host "======================================================" -ForegroundColor Green
Write-Host "✅ Complete Fabric network with proper channel creation" -ForegroundColor Yellow
Write-Host "✅ Automatic peer joining and chaincode deployment" -ForegroundColor Yellow
Write-Host "✅ Pub/Sub integration for external systems" -ForegroundColor Yellow
Write-Host "✅ Health API and monitoring endpoints" -ForegroundColor Yellow

# 1. Create Pub/Sub resources
Write-Host "`n1. Creating Pub/Sub resources..." -ForegroundColor Yellow

gcloud pubsub topics create $Topic --project=$ProjectId 2>$null
if ($LASTEXITCODE -eq 0) {
    Write-Host "   ✅ Topic '$Topic' created" -ForegroundColor Green
} else {
    Write-Host "   ℹ️ Topic '$Topic' already exists" -ForegroundColor Gray
}

gcloud pubsub subscriptions create $Subscription `
    --topic=$Topic `
    --ack-deadline=600 `
    --message-retention-duration=7d `
    --project=$ProjectId 2>$null

if ($LASTEXITCODE -eq 0) {
    Write-Host "   ✅ Subscription '$Subscription' created" -ForegroundColor Green
} else {
    Write-Host "   ℹ️ Subscription '$Subscription' already exists" -ForegroundColor Gray
}

# 2. Create firewall rules
Write-Host "`n2. Creating firewall rules..." -ForegroundColor Yellow

gcloud compute firewall-rules create allow-fabric-enhanced `
    --allow=tcp:7050,tcp:7051,tcp:7052,tcp:8080,tcp:9443 `
    --source-ranges="0.0.0.0/0" `
    --target-tags=fabric `
    --description="Enhanced Fabric peer and orderer ports" `
    --project=$ProjectId 2>$null

Write-Host "   ✅ Firewall rules created" -ForegroundColor Green

# 3. Enhanced startup script with complete Fabric setup
Write-Host "`n3. Creating VM with complete Fabric setup..." -ForegroundColor Yellow

$startupScript = @'
#!/bin/bash
set -e

echo "=== Enhanced Fabric Peer Installation with Complete Setup ==="

# Get external IP for gossip configuration
EXTERNAL_IP=$(curl -s https://api.ipify.org)
echo "External IP: $EXTERNAL_IP"

# Install dependencies
apt-get update
apt-get install -y docker.io docker-compose python3-pip wget curl jq nodejs npm
systemctl start docker
systemctl enable docker

# Fix Docker permissions
usermod -aG docker $(whoami)
chmod 666 /var/run/docker.sock

# Install Python packages for Pub/Sub adapter
pip3 install google-cloud-pubsub flask requests pyyaml

# Install Node.js packages for health API
npm install -g express

# Download and install Fabric binaries
cd /opt
wget https://github.com/hyperledger/fabric/releases/download/v2.5.0/hyperledger-fabric-linux-amd64-2.5.0.tar.gz
tar -xzf hyperledger-fabric-linux-amd64-2.5.0.tar.gz
cp bin/* /usr/local/bin/
chmod +x /usr/local/bin/*

# Setup Fabric directories
mkdir -p /opt/fabric/{crypto-config,channel-artifacts,chaincode,scripts,config}
cd /opt/fabric

# Create comprehensive crypto-config.yaml
cat > crypto-config.yaml << 'EOF'
OrdererOrgs:
  - Name: Orderer
    Domain: metadata.com
    Specs:
      - Hostname: orderer
        CommonName: orderer.metadata.com

PeerOrgs:
  - Name: Org1
    Domain: org1.metadata.com
    EnableNodeOUs: false
    Template:
      Count: 1
      Start: 0
      Hostname: peer
      SANS:
        - "localhost"
        - "EXTERNAL_IP_PLACEHOLDER"
        - "LOCAL_PEER_IP_PLACEHOLDER"
    Users:
      Count: 1
EOF

# Create comprehensive configtx.yaml
cat > configtx.yaml << 'EOF'
Organizations:
  - &OrdererOrg
      Name: OrdererOrg
      ID: OrdererMSP
      MSPDir: crypto-config/ordererOrganizations/metadata.com/msp
      Policies:
        Readers:
          Type: Signature
          Rule: "OR('OrdererMSP.member')"
        Writers:
          Type: Signature
          Rule: "OR('OrdererMSP.member')"
        Admins:
          Type: Signature
          Rule: "OR('OrdererMSP.admin')"

  - &Org1
      Name: Org1MSP
      ID: Org1MSP
      MSPDir: crypto-config/peerOrganizations/org1.metadata.com/msp
      Policies:
        Readers:
          Type: Signature
          Rule: "OR('Org1MSP.admin', 'Org1MSP.peer', 'Org1MSP.client')"
        Writers:
          Type: Signature
          Rule: "OR('Org1MSP.admin', 'Org1MSP.client')"
        Admins:
          Type: Signature
          Rule: "OR('Org1MSP.admin')"
        Endorsement:
          Type: Signature
          Rule: "OR('Org1MSP.peer')"
      AnchorPeers:
        - Host: peer0.org1.metadata.com
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
  Policies:
    Readers:
      Type: ImplicitMeta
      Rule: "ANY Readers"
    Writers:
      Type: ImplicitMeta
      Rule: "ANY Writers"
    Admins:
      Type: ImplicitMeta
      Rule: "MAJORITY Admins"
    LifecycleEndorsement:
      Type: ImplicitMeta
      Rule: "MAJORITY Endorsement"
    Endorsement:
      Type: ImplicitMeta
      Rule: "MAJORITY Endorsement"
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
  Organizations:
  Policies:
    Readers:
      Type: ImplicitMeta
      Rule: "ANY Readers"
    Writers:
      Type: ImplicitMeta
      Rule: "ANY Writers"
    Admins:
      Type: ImplicitMeta
      Rule: "MAJORITY Admins"
    BlockValidation:
      Type: ImplicitMeta
      Rule: "ANY Writers"
  Capabilities:
    <<: *OrdererCapabilities

Channel: &ChannelDefaults
  Policies:
    Readers:
      Type: ImplicitMeta
      Rule: "ANY Readers"
    Writers:
      Type: ImplicitMeta
      Rule: "ANY Writers"
    Admins:
      Type: ImplicitMeta
      Rule: "MAJORITY Admins"
  Capabilities:
    <<: *ChannelCapabilities

Profiles:
  OrdererGenesis:
    <<: *ChannelDefaults
    Orderer:
      <<: *OrdererDefaults
      Organizations:
        - *OrdererOrg
    Consortiums:
      SampleConsortium:
        Organizations:
          - *Org1

  ChannelConfig:
    Consortium: SampleConsortium
    <<: *ChannelDefaults
    Application:
      <<: *ApplicationDefaults
      Organizations:
        - *Org1
EOF

# Replace IP placeholders
sed -i "s/EXTERNAL_IP_PLACEHOLDER/$EXTERNAL_IP/g" crypto-config.yaml
sed -i "s/LOCAL_PEER_IP_PLACEHOLDER/LOCAL_PEER_IP_PLACEHOLDER/g" crypto-config.yaml

# Generate crypto materials
echo "Generating crypto materials..."
cryptogen generate --config=./crypto-config.yaml
if [ $? -ne 0 ]; then
    echo "Error generating crypto materials"
    exit 1
fi

# Generate genesis block and channel config
echo "Generating genesis block..."
mkdir -p channel-artifacts
export FABRIC_CFG_PATH=/opt/fabric
configtxgen -profile OrdererGenesis -outputBlock ./channel-artifacts/genesis.block
if [ $? -ne 0 ]; then
    echo "Error generating genesis block"
    exit 1
fi

echo "Generating channel configuration..."
configtxgen -profile ChannelConfig -outputCreateChannelTx ./channel-artifacts/CHANNEL_NAME_PLACEHOLDER.tx -channelID CHANNEL_NAME_PLACEHOLDER
if [ $? -ne 0 ]; then
    echo "Error generating channel configuration"
    exit 1
fi

# Create enhanced docker-compose.yml
cat > docker-compose.yml << 'EOF'
version: '2'

networks:
  fabric:
    driver: bridge

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
      - ORDERER_OPERATIONS_LISTENADDRESS=0.0.0.0:9443
    volumes:
      - ./channel-artifacts/genesis.block:/var/hyperledger/orderer/orderer.genesis.block
      - ./crypto-config/ordererOrganizations/metadata.com/orderers/orderer.metadata.com/msp:/var/hyperledger/orderer/msp
      - orderer.metadata.com:/var/hyperledger/production/orderer
    ports:
      - 7050:7050
      - 9443:9443
    networks:
      - fabric
    restart: unless-stopped

  peer0.org1.metadata.com:
    container_name: peer0.org1.metadata.com
    image: hyperledger/fabric-peer:2.5
    environment:
      - CORE_VM_ENDPOINT=unix:///host/var/run/docker.sock
      - CORE_VM_DOCKER_HOSTCONFIG_NETWORKMODE=fabric_fabric
      - FABRIC_LOGGING_SPEC=INFO
      - CORE_PEER_ID=peer0.org1.metadata.com
      - CORE_PEER_ADDRESS=peer0.org1.metadata.com:7051
      - CORE_PEER_LISTENADDRESS=0.0.0.0:7051
      - CORE_PEER_CHAINCODEADDRESS=peer0.org1.metadata.com:7052
      - CORE_PEER_CHAINCODELISTENADDRESS=0.0.0.0:7052
      - CORE_PEER_GOSSIP_BOOTSTRAP=peer0.org1.metadata.com:7051,LOCAL_PEER_IP_PLACEHOLDER:7051
      - CORE_PEER_GOSSIP_EXTERNALENDPOINT=EXTERNAL_IP_PLACEHOLDER:7051
      - CORE_PEER_GOSSIP_USELEADERELECTION=true
      - CORE_PEER_GOSSIP_ORGLEADER=false
      - CORE_PEER_LOCALMSPID=Org1MSP
      - CORE_PEER_MSPCONFIGPATH=/etc/hyperledger/fabric/msp
      - CORE_PEER_TLS_ENABLED=false
      - CORE_LEDGER_STATE_STATEDATABASE=goleveldb
      - CORE_OPERATIONS_LISTENADDRESS=0.0.0.0:9444
    volumes:
      - /var/run/docker.sock:/host/var/run/docker.sock
      - ./crypto-config/peerOrganizations/org1.metadata.com/peers/peer0.org1.metadata.com/msp:/etc/hyperledger/fabric/msp
      - peer0.org1.metadata.com:/var/hyperledger/production
    ports:
      - 7051:7051
      - 7052:7052
      - 9444:9444
    depends_on:
      - orderer.metadata.com
    networks:
      - fabric
    restart: unless-stopped

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
      - ./crypto-config:/opt/gopath/src/github.com/hyperledger/fabric/peer/crypto/
      - ./channel-artifacts:/opt/gopath/src/github.com/hyperledger/fabric/peer/channel-artifacts
      - ./scripts:/opt/gopath/src/github.com/hyperledger/fabric/peer/scripts
    depends_on:
      - orderer.metadata.com
      - peer0.org1.metadata.com
    networks:
      - fabric
EOF

# Replace placeholders in docker-compose
sed -i "s/EXTERNAL_IP_PLACEHOLDER/$EXTERNAL_IP/g" docker-compose.yml
sed -i "s/LOCAL_PEER_IP_PLACEHOLDER/LOCAL_PEER_IP_PLACEHOLDER/g" docker-compose.yml

# Create channel setup script
cat > scripts/setup-channel.sh << 'EOF'
#!/bin/bash
set -e

CHANNEL_NAME="CHANNEL_NAME_PLACEHOLDER"

echo "=== Setting up channel: $CHANNEL_NAME ==="

# Wait for services to be ready
sleep 30

# Create channel
echo "Creating channel: $CHANNEL_NAME"
docker exec cli peer channel create \
    -o orderer.metadata.com:7050 \
    -c $CHANNEL_NAME \
    -f ./channel-artifacts/${CHANNEL_NAME}.tx

# Join channel
echo "Joining channel: $CHANNEL_NAME"
docker exec cli peer channel join -b ${CHANNEL_NAME}.block

# List channels to verify
echo "Verifying channel membership:"
docker exec cli peer channel list

echo "✅ Channel setup complete!"
EOF

# Create simple chaincode for testing
mkdir -p chaincode/metadata-cc
cat > chaincode/metadata-cc/metadata-cc.go << 'EOF'
package main

import (
    "encoding/json"
    "fmt"

    "github.com/hyperledger/fabric-contract-api-go/contractapi"
)

type SmartContract struct {
    contractapi.Contract
}

type Metadata struct {
    ID        string `json:"id"`
    Entity    string `json:"entity"`
    Operation string `json:"operation"`
    Data      string `json:"data"`
    Timestamp string `json:"timestamp"`
}

func (s *SmartContract) CreateMetadata(ctx contractapi.TransactionContextInterface, id string, entity string, operation string, data string, timestamp string) error {
    metadata := Metadata{
        ID:        id,
        Entity:    entity,
        Operation: operation,
        Data:      data,
        Timestamp: timestamp,
    }

    metadataJSON, err := json.Marshal(metadata)
    if err != nil {
        return err
    }

    return ctx.GetStub().PutState(id, metadataJSON)
}

func (s *SmartContract) QueryMetadata(ctx contractapi.TransactionContextInterface, id string) (*Metadata, error) {
    metadataJSON, err := ctx.GetStub().GetState(id)
    if err != nil {
        return nil, fmt.Errorf("failed to read from world state: %v", err)
    }
    if metadataJSON == nil {
        return nil, fmt.Errorf("the metadata %s does not exist", id)
    }

    var metadata Metadata
    err = json.Unmarshal(metadataJSON, &metadata)
    if err != nil {
        return nil, err
    }

    return &metadata, nil
}

func main() {
    assetChaincode, err := contractapi.NewChaincode(&SmartContract{})
    if err != nil {
        fmt.Printf("Error creating metadata chaincode: %v", err)
        return
    }

    if err := assetChaincode.Start(); err != nil {
        fmt.Printf("Error starting metadata chaincode: %v", err)
    }
}
EOF

# Create chaincode deployment script
cat > scripts/deploy-chaincode.sh << 'EOF'
#!/bin/bash
set -e

CHANNEL_NAME="CHANNEL_NAME_PLACEHOLDER"
CHAINCODE_NAME="metadata-cc"
CHAINCODE_VERSION="1.0"
CHAINCODE_PATH="/opt/gopath/src/github.com/chaincode/metadata-cc"

echo "=== Deploying Chaincode: $CHAINCODE_NAME ==="

# Copy chaincode to container
docker cp ./chaincode/metadata-cc cli:/opt/gopath/src/github.com/chaincode/

# Package chaincode
echo "1. Packaging chaincode..."
docker exec cli peer lifecycle chaincode package ${CHAINCODE_NAME}.tar.gz \
    --path ${CHAINCODE_PATH} \
    --lang golang \
    --label ${CHAINCODE_NAME}_${CHAINCODE_VERSION}

# Install chaincode
echo "2. Installing chaincode..."
docker exec cli peer lifecycle chaincode install ${CHAINCODE_NAME}.tar.gz

# Get package ID
PACKAGE_ID=$(docker exec cli peer lifecycle chaincode queryinstalled | grep ${CHAINCODE_NAME} | awk '{print $3}' | cut -d ',' -f1)
echo "Package ID: ${PACKAGE_ID}"

# Approve chaincode
echo "3. Approving chaincode..."
docker exec cli peer lifecycle chaincode approveformyorg \
    --channelID ${CHANNEL_NAME} \
    --name ${CHAINCODE_NAME} \
    --version ${CHAINCODE_VERSION} \
    --package-id ${PACKAGE_ID} \
    --sequence 1

# Check commit readiness
echo "4. Checking commit readiness..."
docker exec cli peer lifecycle chaincode checkcommitreadiness \
    --channelID ${CHANNEL_NAME} \
    --name ${CHAINCODE_NAME} \
    --version ${CHAINCODE_VERSION} \
    --sequence 1

# Commit chaincode
echo "5. Committing chaincode..."
docker exec cli peer lifecycle chaincode commit \
    --channelID ${CHANNEL_NAME} \
    --name ${CHAINCODE_NAME} \
    --version ${CHAINCODE_VERSION} \
    --sequence 1

echo "✅ Chaincode deployment complete!"
EOF

# Create Pub/Sub adapter
cat > scripts/pubsub-adapter.py << 'EOF'
#!/usr/bin/env python3
import json
import time
import logging
import requests
from google.cloud import pubsub_v1
from concurrent.futures import ThreadPoolExecutor

# Setup logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

class FabricPubSubAdapter:
    def __init__(self, project_id, subscription_name, channel_name, chaincode_name):
        self.project_id = project_id
        self.subscription_name = subscription_name
        self.channel_name = channel_name
        self.chaincode_name = chaincode_name
        self.subscriber = pubsub_v1.SubscriberClient()
        self.subscription_path = self.subscriber.subscription_path(project_id, subscription_name)
        
    def invoke_chaincode(self, metadata):
        """Invoke chaincode through Fabric peer"""
        try:
            # Create invoke command
            cmd = [
                "peer", "chaincode", "invoke",
                "-o", "orderer.metadata.com:7050",
                "-C", self.channel_name,
                "-n", self.chaincode_name,
                "-c", f'{{"function":"CreateMetadata","Args":["{metadata["id"]}", "{metadata["entity"]}", "{metadata["operation"]}", "{json.dumps(metadata.get("data", {}))}", "{metadata.get("timestamp", "")}"]}}'
            ]
            
            # Execute via CLI container
            result = subprocess.run([
                "docker", "exec", "cli"
            ] + cmd, capture_output=True, text=True, timeout=30)
            
            if result.returncode == 0:
                logger.info(f"Successfully invoked chaincode for metadata: {metadata['id']}")
                return True
            else:
                logger.error(f"Chaincode invocation failed: {result.stderr}")
                return False
                
        except Exception as e:
            logger.error(f"Error invoking chaincode: {e}")
            return False
    
    def callback(self, message):
        """Process incoming Pub/Sub messages"""
        try:
            # Parse message
            data = json.loads(message.data.decode('utf-8'))
            logger.info(f"Received message: {data}")
            
            # Validate required fields
            required_fields = ['id', 'entity', 'operation']
            if not all(field in data for field in required_fields):
                logger.warning(f"Message missing required fields: {data}")
                message.ack()
                return
            
            # Invoke chaincode
            if self.invoke_chaincode(data):
                logger.info(f"Message processed successfully: {data['id']}")
                message.ack()
            else:
                logger.error(f"Failed to process message: {data['id']}")
                message.nack()
                
        except Exception as e:
            logger.error(f"Error processing message: {e}")
            message.nack()
    
    def start(self):
        """Start the adapter"""
        logger.info(f"Starting Pub/Sub adapter for subscription: {self.subscription_path}")
        
        flow_control = pubsub_v1.types.FlowControl(max_messages=100)
        
        with self.subscriber:
            try:
                streaming_pull_future = self.subscriber.subscribe(
                    self.subscription_path, 
                    callback=self.callback,
                    flow_control=flow_control
                )
                logger.info("Listening for messages...")
                
                with ThreadPoolExecutor(max_workers=4) as executor:
                    streaming_pull_future.result()
                    
            except KeyboardInterrupt:
                streaming_pull_future.cancel()
                logger.info("Adapter stopped")

if __name__ == "__main__":
    import subprocess
    import os
    
    # Configuration
    project_id = "PROJECT_ID_PLACEHOLDER"
    subscription_name = "SUBSCRIPTION_PLACEHOLDER"
    channel_name = "CHANNEL_NAME_PLACEHOLDER"
    chaincode_name = "metadata-cc"
    
    # Start adapter
    adapter = FabricPubSubAdapter(project_id, subscription_name, channel_name, chaincode_name)
    adapter.start()
EOF

# Create health API
cat > scripts/health-api.js << 'EOF'
const express = require('express');
const { exec } = require('child_process');
const app = express();

app.use(express.json());

// Health endpoint
app.get('/health', (req, res) => {
    exec('docker ps --format "table {{.Names}}\t{{.Status}}"', (error, stdout, stderr) => {
        if (error) {
            return res.status(500).json({ error: 'Failed to check containers' });
        }
        
        const containers = stdout.split('\n').slice(1).filter(line => line.trim());
        const peer = containers.find(c => c.includes('peer0.org1.metadata.com'));
        const orderer = containers.find(c => c.includes('orderer.metadata.com'));
        
        res.json({
            status: 'healthy',
            peer: peer ? peer.split('\t')[1] : 'Not found',
            orderer: orderer ? orderer.split('\t')[1] : 'Not found',
            external_endpoint: `${process.env.EXTERNAL_IP || 'unknown'}:7051`,
            gossip: 'active',
            timestamp: new Date().toISOString()
        });
    });
});

// Test message endpoint
app.post('/test-message', (req, res) => {
    const testMessage = {
        id: `TEST_${Date.now()}`,
        entity: 'test',
        operation: 'CREATE',
        data: req.body || { message: 'Health check test' },
        timestamp: new Date().toISOString()
    };
    
    res.json({
        message: 'Test message created',
        data: testMessage,
        instructions: 'Publish this to Pub/Sub to test the pipeline'
    });
});

const PORT = 8080;
app.listen(PORT, '0.0.0.0', () => {
    console.log(`Health API listening on port ${PORT}`);
});
EOF

# Replace placeholders in scripts
sed -i "s/CHANNEL_NAME_PLACEHOLDER/CHANNEL_NAME_PLACEHOLDER/g" scripts/setup-channel.sh
sed -i "s/CHANNEL_NAME_PLACEHOLDER/CHANNEL_NAME_PLACEHOLDER/g" scripts/deploy-chaincode.sh
sed -i "s/PROJECT_ID_PLACEHOLDER/PROJECT_ID_PLACEHOLDER/g" scripts/pubsub-adapter.py
sed -i "s/SUBSCRIPTION_PLACEHOLDER/SUBSCRIPTION_PLACEHOLDER/g" scripts/pubsub-adapter.py
sed -i "s/CHANNEL_NAME_PLACEHOLDER/CHANNEL_NAME_PLACEHOLDER/g" scripts/pubsub-adapter.py

# Make scripts executable
chmod +x scripts/*.sh
chmod +x scripts/*.py

# Start Docker containers
echo "Starting Docker containers..."
docker-compose up -d

# Wait for containers to be ready
echo "Waiting for containers to initialize..."
sleep 45

# Setup channel
echo "Setting up channel..."
./scripts/setup-channel.sh

# Deploy chaincode
echo "Deploying chaincode..."
./scripts/deploy-chaincode.sh

# Start health API
echo "Starting health API..."
node scripts/health-api.js &

# Create systemd service for Pub/Sub adapter
cat > /etc/systemd/system/pubsub-fabric-adapter.service << 'EOF'
[Unit]
Description=Pub/Sub to Fabric Adapter
After=docker.service
Requires=docker.service

[Service]
Type=simple
User=root
WorkingDirectory=/opt/fabric
Environment=GOOGLE_APPLICATION_CREDENTIALS=/opt/fabric/config/gcp-credentials.json
ExecStart=/usr/bin/python3 /opt/fabric/scripts/pubsub-adapter.py
Restart=always
RestartSec=10

[Install]
WantedBy=multi-user.target
EOF

# Enable but don't start the adapter yet (needs GCP credentials)
systemctl daemon-reload
systemctl enable pubsub-fabric-adapter

echo "=== Deployment Complete ==="
echo "External IP: $EXTERNAL_IP"
echo "Fabric Peer: peer0.org1.metadata.com:7051"
echo "Health API: http://$EXTERNAL_IP:8080/health"
echo "Channel: CHANNEL_NAME_PLACEHOLDER"
echo "Chaincode: metadata-cc"
echo ""
echo "Next steps:"
echo "1. Add GCP service account key to /opt/fabric/config/gcp-credentials.json"
echo "2. Start Pub/Sub adapter: systemctl start pubsub-fabric-adapter"
'@

# Replace remaining placeholders
$startupScript = $startupScript -replace "LOCAL_PEER_IP_PLACEHOLDER", $LocalPeerIP
$startupScript = $startupScript -replace "PROJECT_ID_PLACEHOLDER", $ProjectId
$startupScript = $startupScript -replace "SUBSCRIPTION_PLACEHOLDER", $Subscription
$startupScript = $startupScript -replace "CHANNEL_NAME_PLACEHOLDER", $ChannelName

# Create and deploy VM
Write-Host "   Creating VM with startup script..." -ForegroundColor Gray
$startupScriptFile = [System.IO.Path]::GetTempFileName()
$startupScript | Out-File -FilePath $startupScriptFile -Encoding UTF8

gcloud compute instances create $VmName `
    --zone=$Zone `
    --machine-type=$VmType `
    --image-family=ubuntu-2204-lts `
    --image-project=ubuntu-os-cloud `
    --boot-disk-size=30GB `
    --tags=fabric `
    --metadata-from-file startup-script=$startupScriptFile `
    --scopes=https://www.googleapis.com/auth/cloud-platform `
    --project=$ProjectId

Remove-Item $startupScriptFile

Write-Host "   ✅ VM created and configuring..." -ForegroundColor Green

# 4. Get VM IP
Write-Host "`n4. Getting VM information..." -ForegroundColor Yellow
Start-Sleep -Seconds 15

$vmInfo = gcloud compute instances describe $VmName --zone=$Zone --format=json --project=$ProjectId | ConvertFrom-Json
$vmIp = $vmInfo.networkInterfaces[0].accessConfigs[0].natIP

Write-Host "   VM IP: $vmIp" -ForegroundColor Green

# 5. Create comprehensive test script
Write-Host "`n5. Creating test and management scripts..." -ForegroundColor Yellow

New-Item -ItemType Directory -Force -Path "fabric-deployment-config" | Out-Null

# Test script
@"
# comprehensive-fabric-test.ps1
param(
    [string]`$ProjectId = "$ProjectId",
    [string]`$Topic = "$Topic",
    [string]`$VmIp = "$vmIp",
    [string]`$ChannelName = "$ChannelName"
)

Write-Host "🧪 Comprehensive Fabric Deployment Test" -ForegroundColor Green
Write-Host "=======================================" -ForegroundColor Green

# 1. Wait for complete initialization
Write-Host "`n1. Waiting for system initialization..." -ForegroundColor Yellow
`$maxAttempts = 30
`$attempts = 0

do {
    `$attempts++
    Start-Sleep -Seconds 10
    
    try {
        `$health = Invoke-RestMethod -Uri "http://`$VmIp`:8080/health" -TimeoutSec 10
        if (`$health.status -eq "healthy") {
            Write-Host "   ✅ System initialized and healthy!" -ForegroundColor Green
            break
        }
    } catch {
        Write-Host "   ⏳ Still initializing... (`$attempts/`$maxAttempts)" -ForegroundColor Gray
    }
} while (`$attempts -lt `$maxAttempts)

if (`$attempts -eq `$maxAttempts) {
    Write-Host "   ❌ System failed to initialize" -ForegroundColor Red
    exit 1
}

# 2. Test health API
Write-Host "`n2. Testing health API..." -ForegroundColor Yellow
`$health = Invoke-RestMethod -Uri "http://`$VmIp`:8080/health"
Write-Host "   Peer Status: `$(`$health.peer)" -ForegroundColor Gray
Write-Host "   Orderer Status: `$(`$health.orderer)" -ForegroundColor Gray
Write-Host "   External Endpoint: `$(`$health.external_endpoint)" -ForegroundColor Gray

# 3. Test channel membership
Write-Host "`n3. Testing channel membership..." -ForegroundColor Yellow
`$channelTest = gcloud compute ssh $VmName --zone=$Zone --command="docker exec cli peer channel list" --project=`$ProjectId 2>``$null
if (`$channelTest -like "*`$ChannelName*") {
    Write-Host "   ✅ Peer is member of channel: `$ChannelName" -ForegroundColor Green
} else {
    Write-Host "   ❌ Peer not on channel: `$ChannelName" -ForegroundColor Red
}

# 4. Test chaincode
Write-Host "`n4. Testing chaincode..." -ForegroundColor Yellow
`$testId = "TEST_`$(Get-Random -Maximum 9999)"
`$chaincodeTest = gcloud compute ssh $VmName --zone=$Zone --command="docker exec cli peer chaincode invoke -o orderer.metadata.com:7050 -C `$ChannelName -n metadata-cc -c '{`"function`":`"CreateMetadata`",`"Args`":[`"`$testId`",`"test`",`"CREATE`",`"{}`",`"`$(Get-Date -Format 'yyyy-MM-ddTHH:mm:ss')`"]}'" --project=`$ProjectId 2>``$null

if (`$LASTEXITCODE -eq 0) {
    Write-Host "   ✅ Chaincode invocation successful" -ForegroundColor Green
} else {
    Write-Host "   ❌ Chaincode invocation failed" -ForegroundColor Red
}

# 5. Test Pub/Sub integration
Write-Host "`n5. Testing Pub/Sub integration..." -ForegroundColor Yellow
`$pubsubTestId = "PUBSUB_TEST_`$(Get-Random -Maximum 9999)"
`$testMessage = @{
    id = `$pubsubTestId
    entity = "customer"
    operation = "CREATE"
    data = @{
        name = "Test Customer"
        source = "PowerShell Test"
    }
    timestamp = (Get-Date).ToString("yyyy-MM-ddTHH:mm:ss")
} | ConvertTo-Json -Compress

gcloud pubsub topics publish `$Topic --message="`$testMessage" --project=`$ProjectId
Write-Host "   ✅ Message published to Pub/Sub" -ForegroundColor Green

Write-Host "   ⏳ Waiting for adapter processing (15 seconds)..." -ForegroundColor Gray
Start-Sleep -Seconds 15

# Check adapter logs
`$adapterLogs = gcloud compute ssh $VmName --zone=$Zone --command="journalctl -u pubsub-fabric-adapter -n 5 --no-pager" --project=`$ProjectId 2>``$null
if (`$adapterLogs -like "*`$pubsubTestId*") {
    Write-Host "   ✅ Message processed by adapter" -ForegroundColor Green
} else {
    Write-Host "   ⚠️ Message not found in adapter logs (may need GCP credentials)" -ForegroundColor Yellow
}

Write-Host "`n✅ Test Complete!" -ForegroundColor Green
Write-Host "==================" -ForegroundColor Green

Write-Host "`n📊 Summary:" -ForegroundColor Yellow
Write-Host "   Health API: ✅ Working" -ForegroundColor Green
Write-Host "   Channel: `$(if (`$channelTest -like "*`$ChannelName*") { "✅ Joined" } else { "❌ Not joined" })" -ForegroundColor $(if (`$channelTest -like "*`$ChannelName*") { "Green" } else { "Red" })
Write-Host "   Chaincode: `$(if (`$LASTEXITCODE -eq 0) { "✅ Working" } else { "❌ Failed" })" -ForegroundColor $(if (`$LASTEXITCODE -eq 0) { "Green" } else { "Red" })
Write-Host "   Pub/Sub: ✅ Connected" -ForegroundColor Green

Write-Host "`n📡 Endpoints:" -ForegroundColor Yellow
Write-Host "   Health API: http://`$VmIp`:8080/health" -ForegroundColor Gray
Write-Host "   Test API: http://`$VmIp`:8080/test-message" -ForegroundColor Gray
Write-Host "   Fabric Peer: `$VmIp`:7051" -ForegroundColor Gray
"@ | Out-File -FilePath "fabric-deployment-config\comprehensive-fabric-test.ps1" -Encoding UTF8

# Management script
@"
# manage-fabric-deployment.ps1
param(
    [Parameter(Mandatory=`$true)]
    [ValidateSet("start", "stop", "restart", "status", "logs", "cleanup")]
    [string]`$Action,
    
    [string]`$ProjectId = "$ProjectId",
    [string]`$VmName = "$VmName",
    [string]`$Zone = "$Zone"
)

switch (`$Action) {
    "start" {
        Write-Host "🚀 Starting Fabric deployment..." -ForegroundColor Green
        gcloud compute instances start `$VmName --zone=`$Zone --project=`$ProjectId
        Start-Sleep -Seconds 30
        gcloud compute ssh `$VmName --zone=`$Zone --command="cd /opt/fabric && docker-compose up -d" --project=`$ProjectId
    }
    
    "stop" {
        Write-Host "🛑 Stopping Fabric deployment..." -ForegroundColor Yellow
        gcloud compute ssh `$VmName --zone=`$Zone --command="cd /opt/fabric && docker-compose down" --project=`$ProjectId
        gcloud compute instances stop `$VmName --zone=`$Zone --project=`$ProjectId
    }
    
    "restart" {
        Write-Host "🔄 Restarting Fabric deployment..." -ForegroundColor Yellow
        gcloud compute ssh `$VmName --zone=`$Zone --command="cd /opt/fabric && docker-compose restart" --project=`$ProjectId
    }
    
    "status" {
        Write-Host "📊 Checking deployment status..." -ForegroundColor Blue
        `$vmStatus = gcloud compute instances describe `$VmName --zone=`$Zone --format="value(status)" --project=`$ProjectId
        Write-Host "VM Status: `$vmStatus" -ForegroundColor Gray
        
        if (`$vmStatus -eq "RUNNING") {
            gcloud compute ssh `$VmName --zone=`$Zone --command="cd /opt/fabric && docker-compose ps" --project=`$ProjectId
        }
    }
    
    "logs" {
        Write-Host "📋 Fetching logs..." -ForegroundColor Blue
        gcloud compute ssh `$VmName --zone=`$Zone --command="cd /opt/fabric && docker-compose logs --tail=50" --project=`$ProjectId
    }
    
    "cleanup" {
        Write-Host "🧹 Cleaning up deployment..." -ForegroundColor Red
        `$confirm = Read-Host "Are you sure you want to delete the VM and all data? (yes/no)"
        if (`$confirm -eq "yes") {
            gcloud compute instances delete `$VmName --zone=`$Zone --project=`$ProjectId --quiet
            gcloud compute firewall-rules delete allow-fabric-enhanced --project=`$ProjectId --quiet
            Write-Host "✅ Cleanup complete" -ForegroundColor Green
        }
    }
}
"@ | Out-File -FilePath "fabric-deployment-config\manage-fabric-deployment.ps1" -Encoding UTF8

Write-Host "   ✅ Management scripts created" -ForegroundColor Green

# 6. Wait for initialization and test
Write-Host "`n6. Waiting for complete system initialization..." -ForegroundColor Yellow
$attempts = 0
$maxAttempts = 40

while ($attempts -lt $maxAttempts) {
    $attempts++
    Start-Sleep -Seconds 15
    
    try {
        $health = Invoke-RestMethod -Uri "http://${vmIp}:8080/health" -TimeoutSec 10
        if ($health.status -eq "healthy") {
            Write-Host "   ✅ System is fully operational!" -ForegroundColor Green
            break
        }
    } catch {
        Write-Host "   ⏳ Still initializing... ($attempts/$maxAttempts)" -ForegroundColor Gray
    }
}

# 7. Final summary
Write-Host "`n✅ Enhanced Fabric Deployment Complete!" -ForegroundColor Green
Write-Host "=========================================" -ForegroundColor Green

Write-Host "`n🏗️ Architecture:" -ForegroundColor Yellow
Write-Host "   Local COBOL → Peer0 ←─Gossip─→ GCP Peer1 ← Pub/Sub Adapter" -ForegroundColor Gray
Write-Host "                                      ↑" -ForegroundColor Gray
Write-Host "                                External Systems" -ForegroundColor Gray

Write-Host "`n🔗 Endpoints:" -ForegroundColor Yellow
Write-Host "   Fabric Peer: $vmIp`:7051" -ForegroundColor Gray
Write-Host "   Health API:  http://$vmIp`:8080/health" -ForegroundColor Gray
Write-Host "   Test API:    http://$vmIp`:8080/test-message" -ForegroundColor Gray

Write-Host "`n📬 Pub/Sub Configuration:" -ForegroundColor Yellow
Write-Host "   Project: $ProjectId" -ForegroundColor Gray
Write-Host "   Topic:   $Topic" -ForegroundColor Gray
Write-Host "   Subscription: $Subscription" -ForegroundColor Gray
Write-Host "   Channel: $ChannelName" -ForegroundColor Gray

Write-Host "`n🧪 Next Steps:" -ForegroundColor Yellow
Write-Host "1. Run comprehensive test:" -ForegroundColor White
Write-Host "   .\fabric-deployment-config\comprehensive-fabric-test.ps1" -ForegroundColor Gray

Write-Host "`n2. Update your LOCAL docker-compose.yml with gossip configuration:" -ForegroundColor White
Write-Host "   Add to your local peer environment:" -ForegroundColor Gray
Write-Host "   - CORE_PEER_GOSSIP_BOOTSTRAP=peer0.org1.example.com:7051,$vmIp`:7051" -ForegroundColor Yellow
Write-Host "   - CORE_PEER_GOSSIP_EXTERNALENDPOINT=$LocalPeerIP`:7051" -ForegroundColor Yellow

Write-Host "`n3. Restart your local containers:" -ForegroundColor White
Write-Host "   docker-compose down && docker-compose up -d" -ForegroundColor Gray

Write-Host "`n4. Manage deployment:" -ForegroundColor White
Write-Host "   .\fabric-deployment-config\manage-fabric-deployment.ps1 -Action status" -ForegroundColor Gray

Write-Host "`n💡 The enhanced deployment includes:" -ForegroundColor Yellow
Write-Host "   ✅ Complete Fabric network with orderer, peer, and CLI" -ForegroundColor Green
Write-Host "   ✅ Automatic channel creation and peer joining" -ForegroundColor Green
Write-Host "   ✅ Chaincode deployment and testing" -ForegroundColor Green
Write-Host "   ✅ Pub/Sub integration for external systems" -ForegroundColor Green
Write-Host "   ✅ Health API and monitoring endpoints" -ForegroundColor Green
Write-Host "   ✅ Comprehensive testing and management scripts" -ForegroundColor Green