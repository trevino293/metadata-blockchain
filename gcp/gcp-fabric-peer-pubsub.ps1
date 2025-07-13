# gcp-fabric-peer-with-pubsub.ps1
# Deploy Fabric peer with Pub/Sub ingestion for external systems

param(
    [Parameter(Mandatory=$true)]
    [string]$ProjectId,
    
    [Parameter(Mandatory=$true)]
    [string]$LocalPeerIP,
    
    [string]$Zone = "us-central1-a",
    [string]$VmName = "fabric-peer",
    [string]$VmType = "e2-medium",
    [string]$Topic = "blockchain-metadata",
    [string]$Subscription = "fabric-ingestion"
)

Write-Host "🚀 Deploying Fabric Peer with Pub/Sub Ingestion" -ForegroundColor Green
Write-Host "================================================" -ForegroundColor Green
Write-Host "✅ True P2P sync between Fabric peers" -ForegroundColor Yellow
Write-Host "✅ Pub/Sub endpoint for external systems" -ForegroundColor Yellow

# 1. Create Pub/Sub resources
Write-Host "`n1. Creating Pub/Sub resources for external ingestion..." -ForegroundColor Yellow

# Create topic
Write-Host "   Creating topic: $Topic" -ForegroundColor Gray
gcloud pubsub topics create $Topic --project=$ProjectId 2>$null
if ($LASTEXITCODE -eq 0) {
    Write-Host "   ✅ Topic created" -ForegroundColor Green
} else {
    Write-Host "   ℹ️ Topic already exists" -ForegroundColor Gray
}

# Create subscription
Write-Host "   Creating subscription: $Subscription" -ForegroundColor Gray
gcloud pubsub subscriptions create $Subscription `
    --topic=$Topic `
    --ack-deadline=600 `
    --message-retention-duration=7d `
    --project=$ProjectId 2>$null

if ($LASTEXITCODE -eq 0) {
    Write-Host "   ✅ Subscription created" -ForegroundColor Green
} else {
    Write-Host "   ℹ️ Subscription already exists" -ForegroundColor Gray
}

# 2. Create firewall rules
Write-Host "`n2. Creating firewall rules..." -ForegroundColor Yellow

gcloud compute firewall-rules create allow-fabric `
    --allow=tcp:7051,tcp:7052,tcp:8080,tcp:9443 `
    --source-ranges="0.0.0.0/0" `
    --target-tags=fabric `
    --description="Fabric peer ports" `
    --project=$ProjectId 2>$null

Write-Host "   ✅ Firewall rules ready" -ForegroundColor Green

# 3. Create VM with enhanced startup script
Write-Host "`n3. Creating VM with Fabric peer and Pub/Sub adapter..." -ForegroundColor Yellow

$startupScript = @'
#!/bin/bash
echo "=== Installing Fabric Peer with Pub/Sub Adapter ==="

# Install dependencies
apt-get update
apt-get install -y docker.io docker-compose python3-pip
systemctl start docker
systemctl enable docker

# Install Python packages
pip3 install google-cloud-pubsub flask

# Setup directories
mkdir -p /opt/fabric/{config,crypto,data,scripts}
cd /opt/fabric

# Create docker-compose.yml for Fabric peer
cat > docker-compose.yml << 'EOF'
version: '3.8'

networks:
  fabric-net:

volumes:
  peer-data:
  orderer-data:

services:
  # Simple orderer for testing (in production, use external orderer)
  orderer.example.com:
    image: hyperledger/fabric-orderer:2.5
    container_name: orderer.example.com
    environment:
      - FABRIC_LOGGING_SPEC=INFO
      - ORDERER_GENERAL_LISTENADDRESS=0.0.0.0
      - ORDERER_GENERAL_LISTENPORT=7050
      - ORDERER_GENERAL_LOCALMSPID=OrdererMSP
      - ORDERER_GENERAL_LOCALMSPDIR=/var/hyperledger/orderer/msp
      - ORDERER_GENERAL_TLS_ENABLED=false
      - ORDERER_GENERAL_GENESISMETHOD=solo
      - ORDERER_GENERAL_GENESISFILE=/var/hyperledger/orderer/orderer.genesis.block
    volumes:
      - ./channel-artifacts/genesis.block:/var/hyperledger/orderer/orderer.genesis.block
      - ./crypto/orderer:/var/hyperledger/orderer/msp
      - orderer-data:/var/hyperledger/production/orderer
    ports:
      - "7050:7050"
    networks:
      - fabric-net

  peer1.org1.example.com:
    image: hyperledger/fabric-peer:2.5
    container_name: peer1.org1.example.com
    environment:
      - FABRIC_LOGGING_SPEC=INFO
      - CORE_PEER_ID=peer1.org1.example.com
      - CORE_PEER_ADDRESS=peer1.org1.example.com:7051
      - CORE_PEER_LISTENADDRESS=0.0.0.0:7051
      - CORE_PEER_CHAINCODEADDRESS=peer1.org1.example.com:7052
      - CORE_PEER_CHAINCODELISTENADDRESS=0.0.0.0:7052
      - CORE_PEER_GOSSIP_BOOTSTRAP=LOCAL_PEER_IP:7051
      - CORE_PEER_GOSSIP_EXTERNALENDPOINT=EXTERNAL_IP:7051
      - CORE_PEER_LOCALMSPID=Org1MSP
      - CORE_PEER_MSPCONFIGPATH=/etc/hyperledger/fabric/msp
      - CORE_PEER_TLS_ENABLED=false
      - CORE_LEDGER_STATE_STATEDATABASE=goleveldb
    volumes:
      - /var/run/docker.sock:/host/var/run/docker.sock
      - ./crypto/msp:/etc/hyperledger/fabric/msp
      - peer-data:/var/hyperledger/production
    ports:
      - "7051:7051"
      - "7052:7052"
    networks:
      - fabric-net
    depends_on:
      - orderer.example.com
    restart: unless-stopped
EOF

# Get external IP and update config
EXTERNAL_IP=$(curl -s http://metadata.google.internal/computeMetadata/v1/instance/network-interfaces/0/access-configs/0/external-ip -H "Metadata-Flavor: Google")
sed -i "s/LOCAL_PEER_IP/LOCAL_PEER_IP_PLACEHOLDER/g" docker-compose.yml
sed -i "s/EXTERNAL_IP/$EXTERNAL_IP/g" docker-compose.yml

# Create Pub/Sub to Fabric adapter
cat > scripts/pubsub-to-fabric.py << 'EOF'
#!/usr/bin/env python3
"""
Pub/Sub to Fabric Adapter
Receives metadata from Pub/Sub and writes to local Fabric peer
"""
import json
import time
import subprocess
import logging
from datetime import datetime
from concurrent.futures import ThreadPoolExecutor
from google.cloud import pubsub_v1

# Configuration
PROJECT_ID = "PROJECT_ID_PLACEHOLDER"
SUBSCRIPTION = "SUBSCRIPTION_PLACEHOLDER"
CHANNEL_NAME = "metadata-channel"
CHAINCODE_NAME = "metadata"

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

subscriber = pubsub_v1.SubscriberClient()
subscription_path = subscriber.subscription_path(PROJECT_ID, SUBSCRIPTION)

def invoke_chaincode(metadata):
    """Submit metadata to local Fabric peer"""
    try:
        # Prepare chaincode arguments
        args = json.dumps({
            "function": "createMetadata",
            "Args": [
                metadata.get("id", f"PS_{int(time.time())}"),
                metadata.get("entity", "external"),
                metadata.get("operation", "CREATE"),
                json.dumps(metadata)
            ]
        })
        
        # Invoke chaincode via docker exec
        cmd = [
            "docker", "exec", "peer1.org1.example.com",
            "peer", "chaincode", "invoke",
            "-o", "orderer.example.com:7050",
            "-C", CHANNEL_NAME,
            "-n", CHAINCODE_NAME,
            "-c", args,
            "--waitForEvent"
        ]
        
        result = subprocess.run(cmd, capture_output=True, text=True, timeout=30)
        
        if result.returncode == 0:
            logger.info(f"✅ Metadata {metadata.get('id')} written to blockchain")
            return True
        else:
            logger.error(f"❌ Chaincode failed: {result.stderr}")
            return False
            
    except subprocess.TimeoutExpired:
        logger.error("⏱️ Chaincode invocation timed out")
        return False
    except Exception as e:
        logger.error(f"❌ Error invoking chaincode: {str(e)}")
        return False

def process_message(message):
    """Process a single Pub/Sub message"""
    try:
        # Parse message
        metadata = json.loads(message.data.decode('utf-8'))
        logger.info(f"📨 Received: {metadata.get('id')} - {metadata.get('entity')}")
        
        # Add metadata
        metadata['source'] = 'pubsub'
        metadata['processed_at'] = datetime.utcnow().isoformat()
        metadata['message_id'] = message.message_id
        
        # Write to blockchain
        success = invoke_chaincode(metadata)
        
        if success:
            message.ack()
            logger.info(f"✅ Message {message.message_id} acknowledged")
        else:
            message.nack()
            logger.warning(f"⚠️ Message {message.message_id} will be retried")
            
    except json.JSONDecodeError as e:
        logger.error(f"❌ Invalid JSON in message: {e}")
        message.ack()  # Ack to prevent poison message
    except Exception as e:
        logger.error(f"❌ Error processing message: {e}")
        message.nack()

def main():
    logger.info("🚀 Starting Pub/Sub to Fabric adapter")
    logger.info(f"📋 Project: {PROJECT_ID}")
    logger.info(f"📬 Subscription: {SUBSCRIPTION}")
    logger.info(f"⛓️ Channel: {CHANNEL_NAME}")
    logger.info(f"📄 Chaincode: {CHAINCODE_NAME}")
    
    # Configure flow control
    flow_control = pubsub_v1.types.FlowControl(max_messages=10)
    
    # Start pulling messages
    with subscriber:
        while True:
            try:
                # Pull messages with callback
                streaming_pull_future = subscriber.subscribe(
                    subscription_path,
                    callback=process_message,
                    flow_control=flow_control
                )
                
                logger.info("👂 Listening for messages...")
                
                # Keep the main thread alive
                with subscriber:
                    try:
                        streaming_pull_future.result()
                    except KeyboardInterrupt:
                        streaming_pull_future.cancel()
                        streaming_pull_future.result()  # Block until cancelled
                        
            except Exception as e:
                logger.error(f"❌ Subscription error: {e}")
                logger.info("🔄 Restarting in 5 seconds...")
                time.sleep(5)

if __name__ == "__main__":
    main()
EOF

# Replace placeholders
sed -i "s/PROJECT_ID_PLACEHOLDER/PROJECT_ID_PLACEHOLDER/g" scripts/pubsub-to-fabric.py
sed -i "s/SUBSCRIPTION_PLACEHOLDER/SUBSCRIPTION_PLACEHOLDER/g" scripts/pubsub-to-fabric.py

chmod +x scripts/pubsub-to-fabric.py

# Create systemd service for Pub/Sub adapter
cat > /etc/systemd/system/pubsub-fabric-adapter.service << 'EOF'
[Unit]
Description=Pub/Sub to Fabric Adapter
After=docker.service
Requires=docker.service

[Service]
Type=simple
WorkingDirectory=/opt/fabric
ExecStart=/usr/bin/python3 /opt/fabric/scripts/pubsub-to-fabric.py
Restart=always
RestartSec=10
User=root
StandardOutput=journal
StandardError=journal

[Install]
WantedBy=multi-user.target
EOF

# Create health check API
cat > scripts/health-api.py << 'EOF'
#!/usr/bin/env python3
from flask import Flask, jsonify, request
import subprocess
import json
from datetime import datetime

app = Flask(__name__)

@app.route('/health', methods=['GET'])
def health():
    """Health check endpoint"""
    try:
        # Check peer status
        peer_result = subprocess.run(
            ['docker', 'ps', '--filter', 'name=peer1', '--format', '{{.Status}}'],
            capture_output=True, text=True
        )
        peer_running = bool(peer_result.stdout.strip())
        
        # Check adapter status
        adapter_result = subprocess.run(
            ['systemctl', 'is-active', 'pubsub-fabric-adapter'],
            capture_output=True, text=True
        )
        adapter_running = adapter_result.stdout.strip() == 'active'
        
        return jsonify({
            'healthy': peer_running and adapter_running,
            'peer': 'running' if peer_running else 'stopped',
            'adapter': 'running' if adapter_running else 'stopped',
            'timestamp': datetime.utcnow().isoformat()
        })
    except Exception as e:
        return jsonify({'error': str(e)}), 500

@app.route('/test-message', methods=['POST'])
def test_message():
    """Send a test message directly to chaincode"""
    try:
        data = request.get_json() or {}
        
        # Create test metadata
        metadata = {
            'id': data.get('id', f'TEST_{int(datetime.now().timestamp())}'),
            'entity': data.get('entity', 'test'),
            'operation': data.get('operation', 'CREATE'),
            'data': data.get('data', {'source': 'health-api'}),
            'timestamp': datetime.utcnow().isoformat()
        }
        
        # Invoke chaincode
        args = json.dumps({
            "function": "createMetadata",
            "Args": [
                metadata['id'],
                metadata['entity'],
                metadata['operation'],
                json.dumps(metadata)
            ]
        })
        
        cmd = [
            "docker", "exec", "peer1.org1.example.com",
            "peer", "chaincode", "invoke",
            "-o", "orderer.example.com:7050",
            "-C", "metadata-channel",
            "-n", "metadata",
            "-c", args
        ]
        
        result = subprocess.run(cmd, capture_output=True, text=True)
        
        if result.returncode == 0:
            return jsonify({
                'success': True,
                'metadata': metadata,
                'message': 'Written to blockchain'
            }), 201
        else:
            return jsonify({
                'success': False,
                'error': result.stderr
            }), 500
            
    except Exception as e:
        return jsonify({'error': str(e)}), 500

if __name__ == '__main__':
    app.run(host='0.0.0.0', port=8080)
EOF

# Start health API in background
python3 scripts/health-api.py &

# Create temporary crypto (will be replaced)
mkdir -p crypto/msp/{admincerts,cacerts,keystore,signcerts}
mkdir -p crypto/orderer/{admincerts,cacerts,keystore,signcerts}
mkdir -p channel-artifacts

# Generate test certificates
openssl req -x509 -newkey rsa:4096 -nodes \
    -keyout crypto/msp/keystore/priv_sk \
    -out crypto/msp/signcerts/cert.pem \
    -days 365 -subj "/C=US/ST=State/L=City/O=Org1/CN=peer1.org1.example.com"

cp crypto/msp/signcerts/cert.pem crypto/msp/cacerts/ca-cert.pem
cp crypto/msp/signcerts/cert.pem crypto/msp/admincerts/admin-cert.pem

# Same for orderer
openssl req -x509 -newkey rsa:4096 -nodes \
    -keyout crypto/orderer/keystore/priv_sk \
    -out crypto/orderer/signcerts/cert.pem \
    -days 365 -subj "/C=US/ST=State/L=City/O=Orderer/CN=orderer.example.com"

cp crypto/orderer/signcerts/cert.pem crypto/orderer/cacerts/ca-cert.pem
cp crypto/orderer/signcerts/cert.pem crypto/orderer/admincerts/admin-cert.pem

# Create MSP config
echo "NodeOUs:" > crypto/msp/config.yaml
echo "  Enable: false" >> crypto/msp/config.yaml
echo "NodeOUs:" > crypto/orderer/config.yaml
echo "  Enable: false" >> crypto/orderer/config.yaml

# Create genesis block (simplified for testing)
touch channel-artifacts/genesis.block

# Update peer IP
sed -i "s/LOCAL_PEER_IP_PLACEHOLDER/LOCAL_PEER_IP_PLACEHOLDER/g" docker-compose.yml

# Start Docker containers
docker-compose up -d

# Wait for containers to start
sleep 30

# Enable and start Pub/Sub adapter
systemctl daemon-reload
systemctl enable pubsub-fabric-adapter
systemctl start pubsub-fabric-adapter

echo "=== Deployment Complete ==="
echo "External IP: $EXTERNAL_IP"
echo "Fabric Peer: peer1.org1.example.com:7051"
echo "Pub/Sub Topic: PROJECT_ID_PLACEHOLDER/topics/TOPIC_PLACEHOLDER"
'@

# Replace placeholders
$startupScript = $startupScript -replace "LOCAL_PEER_IP_PLACEHOLDER", $LocalPeerIP
$startupScript = $startupScript -replace "PROJECT_ID_PLACEHOLDER", $ProjectId
$startupScript = $startupScript -replace "SUBSCRIPTION_PLACEHOLDER", $Subscription
$startupScript = $startupScript -replace "TOPIC_PLACEHOLDER", $Topic

# Create VM
$startupScriptFile = [System.IO.Path]::GetTempFileName()
$startupScript | Out-File -FilePath $startupScriptFile -Encoding UTF8

gcloud compute instances create $VmName `
    --zone=$Zone `
    --machine-type=$VmType `
    --image-family=ubuntu-2204-lts `
    --image-project=ubuntu-os-cloud `
    --boot-disk-size=20GB `
    --tags=fabric `
    --metadata-from-file startup-script=$startupScriptFile `
    --scopes=https://www.googleapis.com/auth/cloud-platform `
    --project=$ProjectId

Remove-Item $startupScriptFile

Write-Host "   ✅ VM created" -ForegroundColor Green

# 4. Wait and get IP
Write-Host "`n4. Waiting for services to start..." -ForegroundColor Yellow
Start-Sleep -Seconds 30

$vmInfo = gcloud compute instances describe $VmName --zone=$Zone --format=json --project=$ProjectId | ConvertFrom-Json
$vmIp = $vmInfo.networkInterfaces[0].accessConfigs[0].natIP

Write-Host "   VM IP: $vmIp" -ForegroundColor Green

# 5. Create test script
Write-Host "`n5. Creating test scripts..." -ForegroundColor Yellow

New-Item -ItemType Directory -Force -Path "fabric-pubsub-config" | Out-Null

# Create test script for Pub/Sub
@"
# test-pubsub-ingestion.ps1
param(
    [string]`$ProjectId = "$ProjectId",
    [string]`$Topic = "$Topic",
    [string]`$VmIp = "$vmIp"
)

Write-Host "Testing Pub/Sub to Fabric Ingestion" -ForegroundColor Green

# 1. Check health
Write-Host "`n1. Checking system health..." -ForegroundColor Yellow
`$health = Invoke-RestMethod -Uri "http://`$VmIp`:8080/health"
Write-Host "   Peer: `$(`$health.peer)" -ForegroundColor Gray
Write-Host "   Adapter: `$(`$health.adapter)" -ForegroundColor Gray

# 2. Send test message via Pub/Sub
Write-Host "`n2. Publishing test message to Pub/Sub..." -ForegroundColor Yellow
`$testMessage = @{
    id = "TEST_`$(Get-Random -Maximum 9999)"
    entity = "customer"
    operation = "CREATE"
    data = @{
        name = "Test Customer"
        source = "PowerShell Test"
        timestamp = (Get-Date).ToString("yyyy-MM-ddTHH:mm:ss")
    }
} | ConvertTo-Json -Compress

gcloud pubsub topics publish `$Topic --message="`$testMessage" --project=`$ProjectId

Write-Host "   ✅ Message published" -ForegroundColor Green

# 3. Wait for processing
Write-Host "`n3. Waiting for blockchain processing (10 seconds)..." -ForegroundColor Yellow
Start-Sleep -Seconds 10

# 4. Check adapter logs
Write-Host "`n4. Recent adapter logs:" -ForegroundColor Yellow
`$logs = gcloud compute ssh `$VmName --zone=$Zone --command="sudo journalctl -u pubsub-fabric-adapter -n 10 --no-pager" --project=`$ProjectId
Write-Host `$logs

Write-Host "`n✅ Test complete!" -ForegroundColor Green
"@ | Out-File -FilePath "fabric-pubsub-config\test-pubsub-ingestion.ps1" -Encoding UTF8

# 6. Wait for full startup
Write-Host "`n6. Waiting for complete initialization..." -ForegroundColor Yellow
$attempts = 0
$maxAttempts = 20

while ($attempts -lt $maxAttempts) {
    $attempts++
    Start-Sleep -Seconds 10
    
    try {
        $health = Invoke-RestMethod -Uri "http://${vmIp}:8080/health" -TimeoutSec 10
        if ($health.healthy) {
            Write-Host "   ✅ System is fully operational!" -ForegroundColor Green
            break
        }
    } catch {
        Write-Host "   ⏳ Still initializing... ($attempts/$maxAttempts)" -ForegroundColor Gray
    }
}

# 7. Summary
Write-Host "`n✅ Deployment Complete!" -ForegroundColor Green
Write-Host "======================" -ForegroundColor Green

Write-Host "`n📊 Architecture:" -ForegroundColor Yellow
Write-Host "   Local COBOL → Peer0 ←─Gossip─→ Peer1 ← Pub/Sub Adapter" -ForegroundColor Gray
Write-Host "                                     ↑" -ForegroundColor Gray
Write-Host "                               External Systems" -ForegroundColor Gray

Write-Host "`n🔗 Endpoints:" -ForegroundColor Yellow
Write-Host "   Fabric Peer: $vmIp`:7051" -ForegroundColor Gray
Write-Host "   Health API:  http://$vmIp`:8080/health" -ForegroundColor Gray
Write-Host "   Test API:    http://$vmIp`:8080/test-message" -ForegroundColor Gray

Write-Host "`n📬 Pub/Sub Configuration:" -ForegroundColor Yellow
Write-Host "   Project: $ProjectId" -ForegroundColor Gray
Write-Host "   Topic:   $Topic" -ForegroundColor Gray
Write-Host "   Subscription: $Subscription" -ForegroundColor Gray

Write-Host "`n🚀 How to use:" -ForegroundColor Yellow
Write-Host "1. External systems publish to Pub/Sub topic:" -ForegroundColor White
Write-Host "   gcloud pubsub topics publish $Topic --message='{`"id`":`"123`",`"entity`":`"customer`"}' --project=$ProjectId" -ForegroundColor Gray

Write-Host "`n2. Test the ingestion pipeline:" -ForegroundColor White
Write-Host "   .\fabric-pubsub-config\test-pubsub-ingestion.ps1" -ForegroundColor Gray

Write-Host "`n3. Next: Sync certificates and create channel:" -ForegroundColor White
Write-Host "   .\sync-crypto-materials.ps1 -ProjectId `"$ProjectId`"" -ForegroundColor Gray
Write-Host "   ./setup-fabric-channel.sh $vmIp" -ForegroundColor Gray

Write-Host "`n💡 The Pub/Sub adapter automatically writes all messages to the blockchain!" -ForegroundColor Green