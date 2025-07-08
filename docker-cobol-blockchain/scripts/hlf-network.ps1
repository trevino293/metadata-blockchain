Write-Host "=== Setting up Hyperledger Fabric Network ===" -ForegroundColor Green

# Step 1: Create network configuration files
Write-Host "`nCreating Fabric network configuration..." -ForegroundColor Yellow

# Create configtx.yaml
@'
Organizations:
  - &OrdererOrg
    Name: OrdererOrg
    ID: OrdererMSP
    MSPDir: crypto-config/ordererOrganizations/example.com/msp
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
    MSPDir: crypto-config/peerOrganizations/org1.example.com/msp
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
    AnchorPeers:
      - Host: peer0.org1.example.com
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
    - orderer.example.com:7050
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
  CobolOrdererGenesis:
    <<: *ChannelDefaults
    Orderer:
      <<: *OrdererDefaults
      Organizations:
        - *OrdererOrg
      Capabilities:
        <<: *OrdererCapabilities
    Consortiums:
      CobolConsortium:
        Organizations:
          - *Org1
  CobolChannel:
    Consortium: CobolConsortium
    <<: *ChannelDefaults
    Application:
      <<: *ApplicationDefaults
      Organizations:
        - *Org1
      Capabilities:
        <<: *ApplicationCapabilities
'@ | Out-File -FilePath "configtx.yaml" -Encoding UTF8

# Create crypto-config.yaml
@'
OrdererOrgs:
  - Name: Orderer
    Domain: example.com
    EnableNodeOUs: true
    Specs:
      - Hostname: orderer

PeerOrgs:
  - Name: Org1
    Domain: org1.example.com
    EnableNodeOUs: true
    Template:
      Count: 1
    Users:
      Count: 1
'@ | Out-File -FilePath "crypto-config.yaml" -Encoding UTF8

# Copy config files to container
docker cp configtx.yaml cobol-metadata-node:/app/
docker cp crypto-config.yaml cobol-metadata-node:/app/

# Step 2: Create Docker Compose for complete Fabric network
Write-Host "`nCreating complete Fabric network configuration..." -ForegroundColor Yellow

@'
version: '3.8'

networks:
  blockchain-net:
    external: true

volumes:
  orderer.example.com:
  peer0.org1.example.com:

services:
  orderer:
    image: hyperledger/fabric-orderer:2.5
    container_name: orderer.example.com
    environment:
      - FABRIC_LOGGING_SPEC=INFO
      - ORDERER_GENERAL_LISTENADDRESS=0.0.0.0
      - ORDERER_GENERAL_LISTENPORT=7050
      - ORDERER_GENERAL_LOCALMSPID=OrdererMSP
      - ORDERER_GENERAL_LOCALMSPDIR=/var/hyperledger/orderer/msp
      - ORDERER_GENERAL_TLS_ENABLED=false
      - ORDERER_GENERAL_GENESISMETHOD=file
      - ORDERER_GENERAL_GENESISFILE=/var/hyperledger/orderer/genesis.block
      - ORDERER_GENERAL_BOOTSTRAPMETHOD=file
    working_dir: /opt/gopath/src/github.com/hyperledger/fabric
    command: orderer
    volumes:
      - ./channel-artifacts/genesis.block:/var/hyperledger/orderer/genesis.block
      - ./crypto-config/ordererOrganizations/example.com/orderers/orderer.example.com/msp:/var/hyperledger/orderer/msp
      - orderer.example.com:/var/hyperledger/production/orderer
    ports:
      - 7050:7050
    networks:
      - blockchain-net

  peer0.org1.example.com:
    image: hyperledger/fabric-peer:2.5
    container_name: peer0.org1.example.com
    environment:
      - CORE_VM_ENDPOINT=unix:///host/var/run/docker.sock
      - CORE_PEER_ID=peer0.org1.example.com
      - CORE_PEER_ADDRESS=peer0.org1.example.com:7051
      - CORE_PEER_LISTENADDRESS=0.0.0.0:7051
      - CORE_PEER_CHAINCODEADDRESS=peer0.org1.example.com:7052
      - CORE_PEER_CHAINCODELISTENADDRESS=0.0.0.0:7052
      - CORE_PEER_GOSSIP_BOOTSTRAP=peer0.org1.example.com:7051
      - CORE_PEER_GOSSIP_EXTERNALENDPOINT=peer0.org1.example.com:7051
      - CORE_PEER_LOCALMSPID=Org1MSP
      - CORE_PEER_MSPCONFIGPATH=/etc/hyperledger/fabric/msp
      - CORE_PEER_TLS_ENABLED=false
      - FABRIC_LOGGING_SPEC=INFO
      - CORE_LEDGER_STATE_STATEDATABASE=CouchDB
      - CORE_LEDGER_STATE_COUCHDBCONFIG_COUCHDBADDRESS=couchdb:5984
      - CORE_LEDGER_STATE_COUCHDBCONFIG_USERNAME=admin
      - CORE_LEDGER_STATE_COUCHDBCONFIG_PASSWORD=adminpw
    volumes:
      - /var/run/docker.sock:/host/var/run/docker.sock
      - ./crypto-config/peerOrganizations/org1.example.com/peers/peer0.org1.example.com/msp:/etc/hyperledger/fabric/msp
      - peer0.org1.example.com:/var/hyperledger/production
    working_dir: /opt/gopath/src/github.com/hyperledger/fabric/peer
    command: peer node start
    ports:
      - 7051:7051
      - 7052:7052
    networks:
      - blockchain-net
    depends_on:
      - orderer
      - couchdb

  couchdb:
    image: couchdb:3.3
    container_name: couchdb
    environment:
      - COUCHDB_USER=admin
      - COUCHDB_PASSWORD=adminpw
    ports:
      - 5984:5984
    networks:
      - blockchain-net

  cli:
    image: hyperledger/fabric-tools:2.5
    container_name: cli
    tty: true
    stdin_open: true
    environment:
      - GOPATH=/opt/gopath
      - CORE_VM_ENDPOINT=unix:///host/var/run/docker.sock
      - FABRIC_LOGGING_SPEC=INFO
      - CORE_PEER_ID=cli
      - CORE_PEER_ADDRESS=peer0.org1.example.com:7051
      - CORE_PEER_LOCALMSPID=Org1MSP
      - CORE_PEER_TLS_ENABLED=false
      - CORE_PEER_MSPCONFIGPATH=/opt/gopath/src/github.com/hyperledger/fabric/peer/crypto/peerOrganizations/org1.example.com/users/Admin@org1.example.com/msp
    working_dir: /opt/gopath/src/github.com/hyperledger/fabric/peer
    command: /bin/bash
    volumes:
      - /var/run/docker.sock:/host/var/run/docker.sock
      - ./chaincode/:/opt/gopath/src/github.com/chaincode
      - ./crypto-config:/opt/gopath/src/github.com/hyperledger/fabric/peer/crypto/
      - ./channel-artifacts:/opt/gopath/src/github.com/hyperledger/fabric/peer/channel-artifacts
    depends_on:
      - orderer
      - peer0.org1.example.com
    networks:
      - blockchain-net

  cobol-blockchain:
    build: .
    container_name: cobol-metadata-node
    volumes:
      - ./data:/app/data
      - ./logs:/app/logs
      - ./chaincode:/app/chaincode
    networks:
      - blockchain-net
    depends_on:
      - peer0.org1.example.com
'@ | Out-File -FilePath "docker-compose-fabric.yml" -Encoding UTF8

# Step 3: Create setup script
Write-Host "`nCreating Fabric setup script..." -ForegroundColor Yellow

@'
#!/bin/bash
# Setup Hyperledger Fabric for COBOL integration

echo "Setting up Hyperledger Fabric network..."

# Create directories
mkdir -p channel-artifacts crypto-config chaincode/cobol-metadata

# Generate crypto materials
echo "Generating crypto materials..."
docker run --rm -v $(pwd):/data hyperledger/fabric-tools:2.5 \
  cryptogen generate --config=/data/crypto-config.yaml --output=/data/crypto-config

# Generate genesis block
echo "Generating genesis block..."
docker run --rm -v $(pwd):/data \
  -e FABRIC_CFG_PATH=/data \
  hyperledger/fabric-tools:2.5 \
  configtxgen -profile CobolOrdererGenesis -channelID system-channel -outputBlock /data/channel-artifacts/genesis.block

# Generate channel configuration
echo "Generating channel configuration..."
docker run --rm -v $(pwd):/data \
  -e FABRIC_CFG_PATH=/data \
  hyperledger/fabric-tools:2.5 \
  configtxgen -profile CobolChannel -outputCreateChannelTx /data/channel-artifacts/cobol-channel.tx -channelID cobol-channel

echo "Fabric network setup complete!"
'@ | Out-File -FilePath "setup-fabric.sh" -Encoding UTF8

# Copy and run setup script
docker cp setup-fabric.sh cobol-metadata-node:/app/
docker exec cobol-metadata-node chmod +x /app/setup-fabric.sh
docker exec cobol-metadata-node dos2unix /app/setup-fabric.sh 2>$null

# Step 4: Create chaincode deployment script
Write-Host "`nCreating chaincode deployment script..." -ForegroundColor Yellow

@'
#!/bin/bash
# Deploy COBOL metadata chaincode to Fabric

CHANNEL_NAME="cobol-channel"
CHAINCODE_NAME="cobol-metadata"
CHAINCODE_VERSION="1.0"
CHAINCODE_PATH="/opt/gopath/src/github.com/chaincode/cobol-metadata"

echo "Deploying COBOL metadata chaincode..."

# Package chaincode
echo "1. Packaging chaincode..."
peer lifecycle chaincode package ${CHAINCODE_NAME}.tar.gz \
  --path ${CHAINCODE_PATH} \
  --lang golang \
  --label ${CHAINCODE_NAME}_${CHAINCODE_VERSION}

# Install chaincode
echo "2. Installing chaincode on peer..."
peer lifecycle chaincode install ${CHAINCODE_NAME}.tar.gz

# Get package ID
PACKAGE_ID=$(peer lifecycle chaincode queryinstalled | grep ${CHAINCODE_NAME} | awk '{print $3}' | cut -d ',' -f1)
echo "Package ID: ${PACKAGE_ID}"

# Approve chaincode
echo "3. Approving chaincode..."
peer lifecycle chaincode approveformyorg \
  --channelID ${CHANNEL_NAME} \
  --name ${CHAINCODE_NAME} \
  --version ${CHAINCODE_VERSION} \
  --package-id ${PACKAGE_ID} \
  --sequence 1

# Commit chaincode
echo "4. Committing chaincode..."
peer lifecycle chaincode commit \
  --channelID ${CHANNEL_NAME} \
  --name ${CHAINCODE_NAME} \
  --version ${CHAINCODE_VERSION} \
  --sequence 1

echo "Chaincode deployment complete!"
'@ | Out-File -FilePath "deploy-chaincode.sh" -Encoding UTF8

# Clean up
Remove-Item "configtx.yaml", "crypto-config.yaml", "docker-compose-fabric.yml", "setup-fabric.sh", "deploy-chaincode.sh" -ErrorAction SilentlyContinue

Write-Host "`n=== Next Steps ===" -ForegroundColor Green
Write-Host "1. The enhanced adapter is now running with Fabric transaction logging" -ForegroundColor Yellow
Write-Host "2. To deploy the actual Fabric network with chaincode:" -ForegroundColor Yellow
Write-Host "   - Stop current containers: docker-compose down" -ForegroundColor Cyan
Write-Host "   - Start full Fabric network: docker-compose -f docker-compose-fabric.yml up -d" -ForegroundColor Cyan
Write-Host "   - Deploy chaincode: docker exec cli /opt/gopath/src/deploy-chaincode.sh" -ForegroundColor Cyan
Write-Host "`n3. Current simulation is logging transactions to:" -ForegroundColor Yellow
Write-Host "   - /app/logs/fabric-transactions.log" -ForegroundColor Cyan
Write-Host "   - /app/logs/blockchain-writes.log" -ForegroundColor Cyan

Write-Host "`nTest the current setup:" -ForegroundColor Green
Write-Host "docker exec cobol-metadata-node python3 /app/scripts/fabric-query.py all" -ForegroundColor Cyan