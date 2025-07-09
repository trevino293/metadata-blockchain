# simple-gcp-deploy.ps1 - Fixed version with proper JSON parsing and error handling

param(
    [Parameter(Mandatory=$true)]
    [string]$ProjectId,
    [string]$Region = "us-central1",
    [string]$Zone = "us-central1-a"
)

Write-Host "🚀 Simple GCP Blockchain Setup (Fixed Version)" -ForegroundColor Green
Write-Host "Project: $ProjectId" -ForegroundColor Yellow
Write-Host ""

# 1. Enable APIs
Write-Host "Enabling APIs..." -ForegroundColor Yellow
gcloud services enable compute.googleapis.com pubsub.googleapis.com --project=$ProjectId

# 2. Create Pub/Sub topic and subscription
Write-Host "Creating Pub/Sub..." -ForegroundColor Yellow
gcloud pubsub topics create metadata-events --project=$ProjectId 2>$null
gcloud pubsub subscriptions create blockchain-sub --topic=metadata-events --project=$ProjectId 2>$null

# 3. Create startup script for VM with FIXED JSON parsing
Write-Host "Creating VM startup script..." -ForegroundColor Yellow

$startupScript = @"
#!/bin/bash
echo "Starting blockchain node setup..."
apt-get update
apt-get install -y python3 python3-pip jq
pip3 install google-cloud-pubsub

# Create FIXED blockchain adapter
cat > /home/blockchain.py << 'EOFPY'
#!/usr/bin/env python3
import json
import time
import hashlib
import base64
import sys
from datetime import datetime
from google.cloud import pubsub_v1
from http.server import HTTPServer, BaseHTTPRequestHandler
import threading
import logging

logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')
logger = logging.getLogger(__name__)

class SimpleBlockchain:
    def __init__(self):
        self.project_id = "$ProjectId"
        self.subscription = f"projects/{self.project_id}/subscriptions/blockchain-sub"
        self.subscriber = pubsub_v1.SubscriberClient()
        self.blocks = []
        self.transactions = []
        self.message_count = 0
        logger.info(f"Initialized blockchain for project: {self.project_id}")
        
    def process_message(self, message):
        try:
            self.message_count += 1
            logger.info(f"Processing message #{self.message_count}")
            
            # Decode message data
            message_data = message.data.decode('utf-8')
            logger.info(f"Raw message data: {repr(message_data)}")
            
            # Try to parse JSON with better error handling
            try:
                data = json.loads(message_data)
                logger.info(f"Parsed JSON successfully: {data}")
            except json.JSONDecodeError as e:
                logger.error(f"JSON decode error: {e}")
                logger.error(f"Problematic data: {repr(message_data)}")
                
                # Try to fix common JSON issues
                fixed_data = message_data.strip()
                if not fixed_data.startswith('{'):
                    # Maybe it's wrapped in quotes
                    if fixed_data.startswith('"') and fixed_data.endswith('"'):
                        fixed_data = fixed_data[1:-1]
                
                # Try again with fixed data
                try:
                    data = json.loads(fixed_data)
                    logger.info(f"Fixed and parsed JSON: {data}")
                except json.JSONDecodeError as e2:
                    logger.error(f"Still can't parse JSON after fixing: {e2}")
                    # Create a simple fallback data structure
                    data = {
                        "operation": "UNKNOWN",
                        "entity": "Message",
                        "id": f"MSG_{self.message_count}",
                        "raw_data": message_data,
                        "error": str(e2)
                    }
                    logger.info(f"Using fallback data: {data}")
            
            # Create transaction
            tx = {
                'id': hashlib.sha256(f"{time.time()}{json.dumps(data)}".encode()).hexdigest()[:16],
                'data': data,
                'timestamp': datetime.now().isoformat(),
                'message_number': self.message_count
            }
            self.transactions.append(tx)
            logger.info(f"✓ Transaction created: {tx['id']} - {data.get('operation', 'UNKNOWN')}")
            
            # Create block every 2 transactions (faster for testing)
            if len(self.transactions) >= 2:
                block = {
                    'number': len(self.blocks) + 1,
                    'timestamp': datetime.now().isoformat(),
                    'transactions': self.transactions.copy(),
                    'hash': hashlib.sha256(json.dumps(self.transactions, default=str).encode()).hexdigest()[:16],
                    'previous_hash': self.blocks[-1]['hash'] if self.blocks else "0000000000000000"
                }
                self.blocks.append(block)
                logger.info(f"✓ Block created: #{block['number']} with hash {block['hash']} ({len(block['transactions'])} transactions)")
                self.transactions.clear()
                
            message.ack()
            logger.info(f"Message #{self.message_count} acknowledged")
            
        except Exception as e:
            logger.error(f"Error processing message #{self.message_count}: {e}")
            logger.error(f"Message data: {repr(message.data)}")
            message.nack()
    
    def start_api(self):
        class Handler(BaseHTTPRequestHandler):
            def __init__(self, blockchain, *args, **kwargs):
                self.blockchain = blockchain
                super().__init__(*args, **kwargs)
                
            def do_GET(self):
                try:
                    self.send_response(200)
                    self.send_header('Content-Type', 'application/json')
                    self.send_header('Access-Control-Allow-Origin', '*')
                    self.end_headers()
                    
                    if self.path == '/status' or self.path == '/health':
                        response = {
                            'status': 'running',
                            'project': self.blockchain.project_id,
                            'blocks': len(self.blockchain.blocks),
                            'pending_transactions': len(self.blockchain.transactions),
                            'total_messages_processed': self.blockchain.message_count,
                            'timestamp': datetime.now().isoformat(),
                            'latest_block_hash': self.blockchain.blocks[-1]['hash'] if self.blockchain.blocks else None
                        }
                    elif self.path == '/blocks':
                        response = {
                            'blocks': self.blockchain.blocks[-10:],  # Last 10 blocks
                            'total_blocks': len(self.blockchain.blocks),
                            'latest_hash': self.blockchain.blocks[-1]['hash'] if self.blockchain.blocks else None
                        }
                    elif self.path == '/transactions':
                        response = {
                            'pending_transactions': self.blockchain.transactions,
                            'total_pending': len(self.blockchain.transactions),
                            'last_transaction_id': self.blockchain.transactions[-1]['id'] if self.blockchain.transactions else None
                        }
                    elif self.path == '/debug':
                        response = {
                            'debug_info': {
                                'project_id': self.blockchain.project_id,
                                'subscription': self.blockchain.subscription,
                                'message_count': self.blockchain.message_count,
                                'blocks_count': len(self.blockchain.blocks),
                                'pending_count': len(self.blockchain.transactions)
                            }
                        }
                    else:
                        response = {
                            'error': 'endpoint not found', 
                            'available_endpoints': ['/status', '/health', '/blocks', '/transactions', '/debug'],
                            'project': self.blockchain.project_id
                        }
                    
                    self.wfile.write(json.dumps(response, indent=2, default=str).encode())
                    
                except Exception as e:
                    logger.error(f"API error: {e}")
                    self.send_response(500)
                    self.send_header('Content-Type', 'application/json')
                    self.end_headers()
                    error_response = {'error': str(e), 'status': 'error'}
                    self.wfile.write(json.dumps(error_response).encode())
            
            def log_message(self, format, *args): 
                pass  # Suppress HTTP logs
        
        try:
            server = HTTPServer(('0.0.0.0', 8080), lambda *args: Handler(self, *args))
            threading.Thread(target=server.serve_forever, daemon=True).start()
            logger.info("🌐 API started on port 8080")
            logger.info("Available endpoints: /status, /health, /blocks, /transactions, /debug")
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
                time.sleep(30)
                # Periodic status
                logger.info(f"Status: {len(self.blocks)} blocks, {len(self.transactions)} pending, {self.message_count} total messages")
                    
        except KeyboardInterrupt:
            logger.info("Shutting down...")
            future.cancel()
        except Exception as e:
            logger.error(f"Runtime error: {e}")
            # Don't exit, keep trying
            time.sleep(10)

if __name__ == "__main__":
    while True:
        try:
            blockchain = SimpleBlockchain()
            blockchain.run()
        except Exception as e:
            logger.error(f"Fatal error: {e}")
            logger.info("Restarting in 10 seconds...")
            time.sleep(10)
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
echo "API endpoints: /status, /health, /blocks, /transactions, /debug"
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
    --project=$ProjectId 2>$null

# 6. Get VM IP with retry logic
Write-Host "Getting VM IP..." -ForegroundColor Yellow
$vmIp = $null
$maxRetries = 5
$retryCount = 0

while ($vmIp -eq $null -and $retryCount -lt $maxRetries) {
    Start-Sleep -Seconds 10
    $retryCount++
    Write-Host "Attempt $retryCount to get VM IP..." -ForegroundColor Gray
    
    try {
        $vmIp = gcloud compute instances describe blockchain-node `
            --zone=$Zone `
            --format="value(networkInterfaces[0].accessConfigs[0].natIP)" `
            --project=$ProjectId 2>$null
        
        if ([string]::IsNullOrWhiteSpace($vmIp)) {
            $vmIp = $null
            Write-Host "VM IP not ready yet..." -ForegroundColor Yellow
        } else {
            Write-Host "Got VM IP: $vmIp" -ForegroundColor Green
        }
    } catch {
        Write-Host "Error getting VM IP: $($_.Exception.Message)" -ForegroundColor Red
    }
}

if ($vmIp -eq $null) {
    Write-Host "❌ Could not get VM IP after $maxRetries attempts" -ForegroundColor Red
    exit 1
}

Write-Host ""
Write-Host "✅ Setup complete!" -ForegroundColor Green
Write-Host "VM IP: $vmIp" -ForegroundColor Cyan
Write-Host "API: http://$vmIp:8080" -ForegroundColor Cyan
Write-Host "Health: http://$vmIp:8080/health" -ForegroundColor Cyan
Write-Host ""
Write-Host "⏳ VM is starting up (wait 3-5 minutes for full initialization)..." -ForegroundColor Yellow
Write-Host ""

# Wait for service to be ready
Write-Host "Waiting for blockchain service to start..." -ForegroundColor Yellow
$serviceReady = $false
$attempts = 0
$maxAttempts = 20

while (-not $serviceReady -and $attempts -lt $maxAttempts) {
    $attempts++
    Start-Sleep -Seconds 15
    
    try {
        $response = Invoke-RestMethod -Uri "http://$vmIp:8080/health" -TimeoutSec 5
        if ($response.status -eq "running") {
            $serviceReady = $true
            Write-Host "✅ Blockchain service is ready!" -ForegroundColor Green
        }
    } catch {
        Write-Host "Service not ready yet (attempt $attempts/$maxAttempts)..." -ForegroundColor Gray
    }
}

Write-Host ""
Write-Host "🧪 Test commands:" -ForegroundColor Green
Write-Host "# Publish test message:" -ForegroundColor Yellow
Write-Host "gcloud pubsub topics publish metadata-events --message='{`"operation`":`"TEST`",`"entity`":`"Demo`",`"id`":`"TEST001`"}' --project=$ProjectId" -ForegroundColor Gray
Write-Host ""
Write-Host "# Check status:" -ForegroundColor Yellow
Write-Host "Invoke-RestMethod -Uri http://$vmIp:8080/status" -ForegroundColor Gray
Write-Host ""
Write-Host "# Monitor logs:" -ForegroundColor Yellow
Write-Host "gcloud compute ssh blockchain-node --zone=$Zone --command='sudo journalctl -u blockchain -f' --project=$ProjectId" -ForegroundColor Gray

# Cleanup temporary file
Remove-Item "startup-script.txt" -ErrorAction SilentlyContinue

# Save connection info to file for later use
@"
# GCP Blockchain Connection Info
Project ID: $ProjectId
VM IP: $vmIp
API Endpoint: http://$vmIp:8080
Health Check: http://$vmIp:8080/health
Debug Info: http://$vmIp:8080/debug
Zone: $Zone

# Quick Commands:
# Test: .\test-simple.ps1 $ProjectId
# Cleanup: .\cleanup-simple.ps1 $ProjectId
# Debug: .\debug-gcp-issues.ps1 $ProjectId
"@ | Out-File -FilePath "connection-info.txt" -Encoding UTF8

Write-Host "Connection info saved to: connection-info.txt" -ForegroundColor Yellow

# Test the deployment immediately
Write-Host ""
Write-Host "🧪 Running quick test..." -ForegroundColor Green

try {
    # Test API
    $status = Invoke-RestMethod -Uri "http://$vmIp:8080/status" -TimeoutSec 10
    Write-Host "✅ API is responding" -ForegroundColor Green
    Write-Host "Status: $($status.status)" -ForegroundColor Cyan
    
    # Publish a test message
    Write-Host "Publishing test message..." -ForegroundColor Yellow
    gcloud pubsub topics publish metadata-events --message='{"operation":"DEPLOY_TEST","entity":"Setup","id":"DEPLOY001","timestamp":"'$(Get-Date -Format "yyyy-MM-ddTHH:mm:ss.fffZ")'"}' --project=$ProjectId
    
    Write-Host "✅ Deployment and test successful!" -ForegroundColor Green
    
} catch {
    Write-Host "⚠️ API not yet ready, but deployment completed successfully" -ForegroundColor Yellow
    Write-Host "Wait a few more minutes and try: Invoke-RestMethod -Uri http://$vmIp:8080/status" -ForegroundColor Gray
}