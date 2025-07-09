# simple-gcp-deploy.ps1 - Deploy with gcloud commands (Windows PowerShell)

param(
    [Parameter(Mandatory=$true)]
    [string]$ProjectId,
    [string]$Region = "us-central1",
    [string]$Zone = "us-central1-a"
)

Write-Host "🚀 Simple GCP Blockchain Setup" -ForegroundColor Green
Write-Host "Project: $ProjectId" -ForegroundColor Yellow
Write-Host ""

# 1. Enable APIs
Write-Host "Enabling APIs..." -ForegroundColor Yellow
gcloud services enable compute.googleapis.com pubsub.googleapis.com --project=$ProjectId

# 2. Create Pub/Sub topic and subscription
Write-Host "Creating Pub/Sub..." -ForegroundColor Yellow
gcloud pubsub topics create metadata-events --project=$ProjectId
gcloud pubsub subscriptions create blockchain-sub --topic=metadata-events --project=$ProjectId

# 3. Create startup script for VM
Write-Host "Creating VM startup script..." -ForegroundColor Yellow

$startupScript = @"
#!/bin/bash
echo "Starting blockchain node setup..."
apt-get update
apt-get install -y python3 python3-pip
pip3 install google-cloud-pubsub

# Create blockchain adapter
cat > /home/blockchain.py << 'EOFPY'
#!/usr/bin/env python3
import json
import time
import hashlib
from datetime import datetime
from google.cloud import pubsub_v1
from http.server import HTTPServer, BaseHTTPRequestHandler
import threading
import logging

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

class SimpleBlockchain:
    def __init__(self):
        self.project_id = "$ProjectId"
        self.subscription = f"projects/{self.project_id}/subscriptions/blockchain-sub"
        self.subscriber = pubsub_v1.SubscriberClient()
        self.blocks = []
        self.transactions = []
        logger.info(f"Initialized blockchain for project: {self.project_id}")
        
    def process_message(self, message):
        try:
            data = json.loads(message.data.decode())
            tx = {
                'id': hashlib.sha256(f"{time.time()}{json.dumps(data)}".encode()).hexdigest()[:16],
                'data': data,
                'timestamp': datetime.now().isoformat()
            }
            self.transactions.append(tx)
            logger.info(f"✓ Transaction: {tx['id']} - {data}")
            
            # Create block every 3 transactions
            if len(self.transactions) >= 3:
                block = {
                    'number': len(self.blocks) + 1,
                    'timestamp': datetime.now().isoformat(),
                    'transactions': self.transactions.copy(),
                    'hash': hashlib.sha256(json.dumps(self.transactions).encode()).hexdigest()[:16]
                }
                self.blocks.append(block)
                logger.info(f"✓ Block created: #{block['number']} with hash {block['hash']}")
                self.transactions.clear()
                
            message.ack()
        except Exception as e:
            logger.error(f"Error processing message: {e}")
            message.nack()
    
    def start_api(self):
        class Handler(BaseHTTPRequestHandler):
            def __init__(self, blockchain, *args, **kwargs):
                self.blockchain = blockchain
                super().__init__(*args, **kwargs)
                
            def do_GET(self):
                try:
                    if self.path == '/status':
                        response = {
                            'status': 'running',
                            'project': self.blockchain.project_id,
                            'blocks': len(self.blockchain.blocks),
                            'pending_transactions': len(self.blockchain.transactions),
                            'timestamp': datetime.now().isoformat()
                        }
                    elif self.path == '/blocks':
                        response = {
                            'blocks': self.blockchain.blocks[-10:],  # Last 10 blocks
                            'total_blocks': len(self.blockchain.blocks)
                        }
                    elif self.path == '/transactions':
                        response = {
                            'pending_transactions': self.blockchain.transactions,
                            'total_pending': len(self.blockchain.transactions)
                        }
                    else:
                        response = {'error': 'endpoint not found', 'available': ['/status', '/blocks', '/transactions']}
                    
                    self.send_response(200)
                    self.send_header('Content-Type', 'application/json')
                    self.send_header('Access-Control-Allow-Origin', '*')
                    self.end_headers()
                    self.wfile.write(json.dumps(response, indent=2).encode())
                except Exception as e:
                    logger.error(f"API error: {e}")
                    self.send_response(500)
                    self.end_headers()
            
            def log_message(self, format, *args): 
                pass  # Suppress HTTP logs
        
        try:
            server = HTTPServer(('0.0.0.0', 8080), lambda *args: Handler(self, *args))
            threading.Thread(target=server.serve_forever, daemon=True).start()
            logger.info("🌐 API started on port 8080")
        except Exception as e:
            logger.error(f"Failed to start API: {e}")
    
    def run(self):
        logger.info("🔗 Starting blockchain...")
        self.start_api()
        
        try:
            future = self.subscriber.subscribe(self.subscription, callback=self.process_message)
            logger.info(f"📡 Listening to: {self.subscription}")
            
            # Keep running
            while True:
                time.sleep(10)
                # Periodic status
                if len(self.blocks) > 0:
                    logger.info(f"Status: {len(self.blocks)} blocks, {len(self.transactions)} pending transactions")
                    
        except KeyboardInterrupt:
            logger.info("Shutting down...")
            future.cancel()
        except Exception as e:
            logger.error(f"Runtime error: {e}")

if __name__ == "__main__":
    blockchain = SimpleBlockchain()
    blockchain.run()
EOFPY

# Make executable and start
chmod +x /home/blockchain.py

# Create systemd service for auto-restart
cat > /etc/systemd/system/blockchain.service << 'EOFSVC'
[Unit]
Description=Simple Blockchain Service
After=network.target

[Service]
Type=simple
User=root
ExecStart=/usr/bin/python3 /home/blockchain.py
Restart=always
RestartSec=10
StandardOutput=journal
StandardError=journal

[Install]
WantedBy=multi-user.target
EOFSVC

# Enable and start service
systemctl daemon-reload
systemctl enable blockchain
systemctl start blockchain

echo "✅ Blockchain service started"
echo "Logs: journalctl -u blockchain -f"
"@

# Save startup script to temporary file
$startupScript | Out-File -FilePath "startup-script.txt" -Encoding UTF8

# 4. Create VM
Write-Host "Creating VM..." -ForegroundColor Yellow
gcloud compute instances create blockchain-node `
    --zone=$Zone `
    --machine-type=e2-small `
    --image-family=ubuntu-2204-lts `
    --image-project=ubuntu-os-cloud `
    --metadata-from-file=startup-script=startup-script.txt `
    --tags=blockchain `
    --project=$ProjectId

# 5. Create firewall rule
Write-Host "Creating firewall rule..." -ForegroundColor Yellow
gcloud compute firewall-rules create allow-blockchain `
    --allow=tcp:8080 `
    --source-ranges=0.0.0.0/0 `
    --target-tags=blockchain `
    --project=$ProjectId

# 6. Get VM IP
Write-Host "Getting VM IP..." -ForegroundColor Yellow
Start-Sleep -Seconds 10  # Wait a moment for VM to get IP

$vmIp = gcloud compute instances describe blockchain-node `
    --zone=$Zone `
    --format="value(networkInterfaces[0].accessConfigs[0].natIP)" `
    --project=$ProjectId

Write-Host ""
Write-Host "✅ Setup complete!" -ForegroundColor Green
Write-Host "VM IP: $vmIp" -ForegroundColor Cyan
Write-Host "API: http://$vmIp:8080" -ForegroundColor Cyan
Write-Host ""
Write-Host "⏳ VM is starting up (wait 2-3 minutes)..." -ForegroundColor Yellow
Write-Host ""
Write-Host "Test commands:" -ForegroundColor Green
Write-Host "gcloud pubsub topics publish metadata-events --message='{`"test`": `"data`"}' --project=$ProjectId" -ForegroundColor Gray
Write-Host "Invoke-RestMethod -Uri http://$vmIp:8080/status" -ForegroundColor Gray
Write-Host ""
Write-Host "Monitor logs:" -ForegroundColor Green
Write-Host "gcloud compute ssh blockchain-node --zone=$Zone --command='sudo journalctl -u blockchain -f' --project=$ProjectId" -ForegroundColor Gray

# Cleanup temporary file
Remove-Item "startup-script.txt" -ErrorAction SilentlyContinue

# Save connection info to file for later use
@"
# GCP Blockchain Connection Info
Project ID: $ProjectId
VM IP: $vmIp
API Endpoint: http://$vmIp:8080
Zone: $Zone

# Quick Commands:
# Test: .\test-simple.ps1 $ProjectId
# Cleanup: .\cleanup-simple.ps1 $ProjectId
"@ | Out-File -FilePath "connection-info.txt" -Encoding UTF8

Write-Host "Connection info saved to: connection-info.txt" -ForegroundColor Yellow to: connection-info.txt" -ForegroundColor Yellown-info.txt" -ForegroundColor Yellow