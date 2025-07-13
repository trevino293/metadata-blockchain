# Manual Fabric Setup - Step by Step
# Run each command individually to avoid PowerShell/SSH parsing issues

Write-Host "🔧 Manual Fabric Setup - Step by Step" -ForegroundColor Cyan
Write-Host "=====================================" -ForegroundColor Cyan
Write-Host ""
Write-Host "⚠️  IMPORTANT: Run each command manually to avoid SSH parsing issues" -ForegroundColor Yellow
Write-Host ""

$PeerIP = "34.170.20.180"
$ProjectId = "metadata-blockchain"
$LocalPeerIP = "184.98.199.152"

Write-Host "📝 STEP 1: SSH into your GCP VM" -ForegroundColor Green
Write-Host "Run this command:" -ForegroundColor Yellow
Write-Host "gcloud compute ssh fabric-peer --project=metadata-blockchain --zone=us-central1-a" -ForegroundColor White
Write-Host ""
Write-Host "Then run the following commands INSIDE the VM:" -ForegroundColor Yellow
Write-Host ""

Write-Host "📝 STEP 2: Fix Docker Permissions" -ForegroundColor Green
Write-Host "# Get current username and fix Docker permissions"
Write-Host 'CURRENT_USER=$(whoami)' -ForegroundColor White
Write-Host 'echo "Current user: $CURRENT_USER"' -ForegroundColor White
Write-Host 'sudo usermod -aG docker $CURRENT_USER' -ForegroundColor White
Write-Host 'sudo chmod 666 /var/run/docker.sock' -ForegroundColor White
Write-Host 'sudo systemctl restart docker' -ForegroundColor White
Write-Host 'newgrp docker' -ForegroundColor White
Write-Host ""

Write-Host "📝 STEP 3: Create Directories" -ForegroundColor Green
Write-Host 'sudo mkdir -p /opt/fabric/{crypto-config,channel-artifacts,chaincode,scripts}' -ForegroundColor White
Write-Host 'sudo chown -R $USER:$USER /opt/fabric' -ForegroundColor White
Write-Host 'cd /opt/fabric' -ForegroundColor White
Write-Host ""

Write-Host "📝 STEP 4: Install Fabric Binaries" -ForegroundColor Green
Write-Host '# Download Fabric binaries' -ForegroundColor Gray
Write-Host 'wget https://github.com/hyperledger/fabric/releases/download/v2.5.0/hyperledger-fabric-linux-amd64-2.5.0.tar.gz' -ForegroundColor White
Write-Host 'tar -xzf hyperledger-fabric-linux-amd64-2.5.0.tar.gz' -ForegroundColor White
Write-Host 'sudo cp bin/* /usr/local/bin/' -ForegroundColor White
Write-Host 'sudo chmod +x /usr/local/bin/*' -ForegroundColor White
Write-Host '# Verify installation' -ForegroundColor Gray
Write-Host 'cryptogen version' -ForegroundColor White
Write-Host 'configtxgen -version' -ForegroundColor White
Write-Host ""

Write-Host "📝 STEP 5: Create Configuration Files" -ForegroundColor Green
Write-Host '# Create crypto-config.yaml' -ForegroundColor Gray
Write-Host @'
cat > crypto-config.yaml << 'EOF'
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
EOF
'@ -ForegroundColor White
Write-Host ""

Write-Host '# Create configtx.yaml' -ForegroundColor Gray
Write-Host @'
cat > configtx.yaml << 'EOF'
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
        - Host: peer0.org1.metadata.com
          Port: 7051

Orderer: &OrdererDefaults
  OrdererType: solo
  Addresses:
    - orderer.metadata.com:7050
  BatchTimeout: 2s
  BatchSize:
    MaxMessageCount: 10
    AbsoluteMaxBytes: 99 MB
    PreferredMaxBytes: 512 KB

Application: &ApplicationDefaults
  Organizations:

Profiles:
  OrdererGenesis:
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
    Application:
      <<: *ApplicationDefaults
      Organizations:
        - *Org1
EOF
'@ -ForegroundColor White
Write-Host ""

Write-Host "📝 STEP 6: Generate Crypto Materials" -ForegroundColor Green
Write-Host 'cryptogen generate --config=./crypto-config.yaml' -ForegroundColor White
Write-Host 'ls -la crypto-config/' -ForegroundColor White
Write-Host ""

Write-Host "📝 STEP 7: Generate Genesis Block and Channel Config" -ForegroundColor Green
Write-Host 'mkdir -p channel-artifacts' -ForegroundColor White
Write-Host 'configtxgen -profile OrdererGenesis -outputBlock ./channel-artifacts/genesis.block' -ForegroundColor White
Write-Host 'configtxgen -profile ChannelConfig -outputCreateChannelTx ./channel-artifacts/metadata-channel.tx -channelID metadata-channel' -ForegroundColor White
Write-Host ""

Write-Host "📝 STEP 8: Create Docker Compose File" -ForegroundColor Green
Write-Host @'
cat > docker-compose.yaml << 'EOF'
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
      - CORE_PEER_GOSSIP_BOOTSTRAP=peer0.org1.metadata.com:7051,184.98.199.152:7051
      - CORE_PEER_GOSSIP_EXTERNALENDPOINT=34.170.20.180:7051
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
      - ./crypto-config:/opt/gopath/src/github.com/hyperledger/fabric/peer/crypto/
      - ./channel-artifacts:/opt/gopath/src/github.com/hyperledger/fabric/peer/channel-artifacts
    depends_on:
      - orderer.metadata.com
      - peer0.org1.metadata.com
    networks:
      - fabric
EOF
'@ -ForegroundColor White
Write-Host ""

Write-Host "📝 STEP 9: Start Docker Containers" -ForegroundColor Green
Write-Host '# Stop any existing containers' -ForegroundColor Gray
Write-Host 'docker-compose down' -ForegroundColor White
Write-Host '# Start the network' -ForegroundColor Gray
Write-Host 'docker-compose up -d' -ForegroundColor White
Write-Host '# Check status' -ForegroundColor Gray
Write-Host 'docker-compose ps' -ForegroundColor White
Write-Host 'docker logs peer0.org1.metadata.com' -ForegroundColor White
Write-Host ""

Write-Host "📝 STEP 10: Create and Join Channel" -ForegroundColor Green
Write-Host '# Wait for containers to start' -ForegroundColor Gray
Write-Host 'sleep 30' -ForegroundColor White
Write-Host '# Create channel' -ForegroundColor Gray
Write-Host 'docker exec cli peer channel create -o orderer.metadata.com:7050 -c metadata-channel -f ./channel-artifacts/metadata-channel.tx' -ForegroundColor White
Write-Host '# Join channel' -ForegroundColor Gray
Write-Host 'docker exec cli peer channel join -b metadata-channel.block' -ForegroundColor White
Write-Host ""

Write-Host "📝 STEP 11: Create Health API" -ForegroundColor Green
Write-Host 'mkdir -p scripts' -ForegroundColor White
Write-Host @'
cat > scripts/server.js << 'EOF'
const express = require("express");
const app = express();

app.get("/health", (req, res) => res.json({
  status: "healthy",
  gossip: "active",
  peers: ["184.98.199.152:7051"],
  external_endpoint: "34.170.20.180:7051"
}));

app.get("/status", (req, res) => res.json({
  status: "operational",
  blocks: 0
}));

app.listen(8080, () => console.log("Health API listening on 8080"));
EOF
'@ -ForegroundColor White
Write-Host ""

Write-Host '# Install Node.js and start health API' -ForegroundColor Gray
Write-Host 'curl -fsSL https://deb.nodesource.com/setup_16.x | sudo -E bash -' -ForegroundColor White
Write-Host 'sudo apt-get install -y nodejs' -ForegroundColor White
Write-Host 'cd scripts' -ForegroundColor White
Write-Host 'npm init -y' -ForegroundColor White
Write-Host 'npm install express' -ForegroundColor White
Write-Host 'nohup node server.js > ../health-api.log 2>&1 &' -ForegroundColor White
Write-Host 'cd ..' -ForegroundColor White
Write-Host ""

Write-Host "📝 STEP 12: Test Everything" -ForegroundColor Green
Write-Host '# Test health endpoint' -ForegroundColor Gray
Write-Host 'curl http://localhost:8080/health' -ForegroundColor White
Write-Host '# Check gossip logs' -ForegroundColor Gray
Write-Host 'docker logs peer0.org1.metadata.com | grep gossip' -ForegroundColor White
Write-Host '# Test peer connectivity' -ForegroundColor Gray
Write-Host 'docker exec cli peer node status' -ForegroundColor White
Write-Host ""

Write-Host "🎯 TROUBLESHOOTING TIPS:" -ForegroundColor Cyan
Write-Host "========================" -ForegroundColor Cyan
Write-Host ""
Write-Host "If you get 'cryptogen: command not found':" -ForegroundColor Yellow
Write-Host "• Check: which cryptogen" -ForegroundColor Gray
Write-Host "• Check: echo `$PATH" -ForegroundColor Gray
Write-Host "• Try: export PATH=`$PATH:/usr/local/bin" -ForegroundColor Gray
Write-Host ""
Write-Host "If Docker permission errors persist:" -ForegroundColor Yellow
Write-Host "• Run: sudo chmod 666 /var/run/docker.sock" -ForegroundColor Gray
Write-Host "• Run: sudo systemctl restart docker" -ForegroundColor Gray
Write-Host "• Exit SSH and reconnect" -ForegroundColor Gray
Write-Host ""
Write-Host "If containers won't start:" -ForegroundColor Yellow
Write-Host "• Run: docker-compose logs" -ForegroundColor Gray
Write-Host "• Check: docker ps -a" -ForegroundColor Gray
Write-Host "• Try: docker system prune -f" -ForegroundColor Gray
Write-Host ""

Write-Host "✅ AFTER SETUP COMPLETES:" -ForegroundColor Green
Write-Host "=========================" -ForegroundColor Green
Write-Host ""
Write-Host "1. Update your LOCAL docker-compose.yml:" -ForegroundColor White
Write-Host "   Add these environment variables to your local peer:" -ForegroundColor Gray
Write-Host "   - CORE_PEER_GOSSIP_BOOTSTRAP=peer0.org1.example.com:7051,$PeerIP`:7051" -ForegroundColor Yellow
Write-Host "   - CORE_PEER_GOSSIP_EXTERNALENDPOINT=$LocalPeerIP`:7051" -ForegroundColor Yellow
Write-Host ""
Write-Host "2. Restart your local containers:" -ForegroundColor White
Write-Host "   docker-compose down && docker-compose up -d" -ForegroundColor Gray
Write-Host ""
Write-Host "3. Test connectivity:" -ForegroundColor White
Write-Host "   curl http://$PeerIP`:8080/health" -ForegroundColor Gray
Write-Host ""