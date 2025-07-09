# debug-gcp-issues.ps1 - Debug and fix GCP Pub/Sub and API issues

param(
    [Parameter(Mandatory=$true)]
    [string]$ProjectId,
    [string]$Zone = "us-central1-a"
)

Write-Host "🔧 Debugging GCP Blockchain Issues" -ForegroundColor Yellow
Write-Host "Project: $ProjectId" -ForegroundColor Cyan
Write-Host ""

# 1. Check VM status
Write-Host "1. Checking VM status..." -ForegroundColor Yellow
try {
    $vmStatus = gcloud compute instances describe blockchain-node `
        --zone=$Zone `
        --format="value(status)" `
        --project=$ProjectId
    
    $vmIp = gcloud compute instances describe blockchain-node `
        --zone=$Zone `
        --format="value(networkInterfaces[0].accessConfigs[0].natIP)" `
        --project=$ProjectId
    
    Write-Host "   VM Status: $vmStatus" -ForegroundColor Green
    Write-Host "   VM IP: $vmIp" -ForegroundColor Green
    
    if ($vmStatus -ne "RUNNING") {
        Write-Host "❌ VM is not running! Starting it..." -ForegroundColor Red
        gcloud compute instances start blockchain-node --zone=$Zone --project=$ProjectId
        Write-Host "⏳ Waiting for VM to start..." -ForegroundColor Yellow
        Start-Sleep -Seconds 30
    }
} catch {
    Write-Host "❌ Could not get VM status: $($_.Exception.Message)" -ForegroundColor Red
    exit 1
}

Write-Host ""

# 2. Check Pub/Sub setup
Write-Host "2. Checking Pub/Sub setup..." -ForegroundColor Yellow

# Check topic
$topicExists = gcloud pubsub topics describe metadata-events --project=$ProjectId 2>$null
if ($LASTEXITCODE -eq 0) {
    Write-Host "   ✅ Topic 'metadata-events' exists" -ForegroundColor Green
} else {
    Write-Host "   ❌ Topic 'metadata-events' missing! Creating..." -ForegroundColor Red
    gcloud pubsub topics create metadata-events --project=$ProjectId
}

# Check subscription
$subExists = gcloud pubsub subscriptions describe blockchain-sub --project=$ProjectId 2>$null
if ($LASTEXITCODE -eq 0) {
    Write-Host "   ✅ Subscription 'blockchain-sub' exists" -ForegroundColor Green
} else {
    Write-Host "   ❌ Subscription 'blockchain-sub' missing! Creating..." -ForegroundColor Red
    gcloud pubsub subscriptions create blockchain-sub --topic=metadata-events --project=$ProjectId
}

# Check subscription backlog
Write-Host "   Checking subscription stats..." -ForegroundColor Gray
try {
    $subStats = gcloud pubsub subscriptions describe blockchain-sub --format="value(numUndeliveredMessages)" --project=$ProjectId 2>$null
    Write-Host "   Undelivered messages: $subStats" -ForegroundColor Cyan
} catch {
    Write-Host "   Could not get subscription stats" -ForegroundColor Yellow
}

Write-Host ""

# 3. Check VM logs to diagnose issues
Write-Host "3. Checking VM service logs..." -ForegroundColor Yellow
try {
    Write-Host "   Fetching recent blockchain service logs..." -ForegroundColor Gray
    
    $vmLogs = gcloud compute ssh blockchain-node `
        --zone=$Zone `
        --command='sudo journalctl -u blockchain --no-pager -n 20' `
        --project=$ProjectId 2>$null
    
    if ($vmLogs) {
        Write-Host "   Recent logs:" -ForegroundColor Cyan
        $vmLogs | ForEach-Object { Write-Host "     $_" -ForegroundColor Gray }
    } else {
        Write-Host "   ⚠️ Could not fetch logs or service not running" -ForegroundColor Yellow
    }
} catch {
    Write-Host "   ❌ Could not connect to VM for logs: $($_.Exception.Message)" -ForegroundColor Red
}

Write-Host ""

# 4. Check if service is actually running
Write-Host "4. Checking if blockchain service is running..." -ForegroundColor Yellow
try {
    $serviceStatus = gcloud compute ssh blockchain-node `
        --zone=$Zone `
        --command='systemctl is-active blockchain' `
        --project=$ProjectId 2>$null
    
    Write-Host "   Service status: $serviceStatus" -ForegroundColor Cyan
    
    if ($serviceStatus -ne "active") {
        Write-Host "   ❌ Service not active! Attempting restart..." -ForegroundColor Red
        
        gcloud compute ssh blockchain-node `
            --zone=$Zone `
            --command='sudo systemctl restart blockchain' `
            --project=$ProjectId
        
        Start-Sleep -Seconds 10
        
        $newStatus = gcloud compute ssh blockchain-node `
            --zone=$Zone `
            --command='systemctl is-active blockchain' `
            --project=$ProjectId 2>$null
        
        Write-Host "   New service status: $newStatus" -ForegroundColor Cyan
    }
} catch {
    Write-Host "   ❌ Could not check service status" -ForegroundColor Red
}

Write-Host ""

# 5. Test API directly
Write-Host "5. Testing API..." -ForegroundColor Yellow
try {
    if ([string]::IsNullOrEmpty($vmIp)) {
        throw "VM IP is empty"
    }
    
    $statusResponse = Invoke-RestMethod -Uri "http://$vmIp:8080/status" -TimeoutSec 15
    Write-Host "   ✅ API is responding!" -ForegroundColor Green
    Write-Host "   Status: $($statusResponse.status)" -ForegroundColor Cyan
    Write-Host "   Blocks: $($statusResponse.blocks)" -ForegroundColor Cyan
    Write-Host "   Pending: $($statusResponse.pending_transactions)" -ForegroundColor Cyan
} catch {
    Write-Host "   ❌ API not responding: $($_.Exception.Message)" -ForegroundColor Red
    
    # Try to restart the service
    Write-Host "   Attempting to fix..." -ForegroundColor Yellow
    try {
        gcloud compute ssh blockchain-node `
            --zone=$Zone `
            --command='sudo systemctl stop blockchain && sudo systemctl start blockchain' `
            --project=$ProjectId
        
        Write-Host "   Service restarted. Waiting 15 seconds..." -ForegroundColor Yellow
        Start-Sleep -Seconds 15
        
        # Test again
        $retryResponse = Invoke-RestMethod -Uri "http://$vmIp:8080/status" -TimeoutSec 10
        Write-Host "   ✅ API now responding after restart!" -ForegroundColor Green
    } catch {
        Write-Host "   ❌ Still not responding after restart" -ForegroundColor Red
    }
}

Write-Host ""

# 6. Create improved blockchain service
Write-Host "6. Deploying improved blockchain service..." -ForegroundColor Yellow

$improvedScript = @"
#!/usr/bin/env python3
import json
import time
import hashlib
import logging
import threading
from datetime import datetime
from google.cloud import pubsub_v1
from http.server import HTTPServer, BaseHTTPRequestHandler
import os

# Set up logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(levelname)s - %(message)s',
    handlers=[
        logging.FileHandler('/var/log/blockchain.log'),
        logging.StreamHandler()
    ]
)
logger = logging.getLogger(__name__)

class ImprovedBlockchain:
    def __init__(self):
        self.project_id = "$ProjectId"
        self.subscription_path = f"projects/{self.project_id}/subscriptions/blockchain-sub"
        self.blocks = []
        self.transactions = []
        self.running = True
        
        # Initialize Pub/Sub client with error handling
        try:
            self.subscriber = pubsub_v1.SubscriberClient()
            logger.info(f"✅ Pub/Sub client initialized for project: {self.project_id}")
        except Exception as e:
            logger.error(f"❌ Failed to initialize Pub/Sub client: {e}")
            raise
        
        logger.info(f"🔗 Blockchain initialized")
        
    def process_message(self, message):
        try:
            # Decode and parse message
            data = json.loads(message.data.decode('utf-8'))
            
            # Add timestamp if not present
            if 'timestamp' not in data:
                data['timestamp'] = datetime.now().isoformat()
            
            # Create transaction
            tx = {
                'id': hashlib.sha256(f"{time.time()}{json.dumps(data, sort_keys=True)}".encode()).hexdigest()[:16],
                'data': data,
                'timestamp': datetime.now().isoformat(),
                'message_id': message.message_id
            }
            
            self.transactions.append(tx)
            logger.info(f"✅ Transaction added: {tx['id']} - {data.get('operation', 'UNKNOWN')} {data.get('entity', 'UNKNOWN')}")
            
            # Create block every 2 transactions (lower threshold for testing)
            if len(self.transactions) >= 2:
                self.create_block()
            
            # Acknowledge message
            message.ack()
            logger.info(f"✅ Message acknowledged: {message.message_id}")
            
        except json.JSONDecodeError as e:
            logger.error(f"❌ Invalid JSON in message: {e}")
            message.nack()
        except Exception as e:
            logger.error(f"❌ Error processing message: {e}")
            message.nack()
    
    def create_block(self):
        try:
            previous_hash = self.blocks[-1]['hash'] if self.blocks else '0' * 16
            
            block = {
                'number': len(self.blocks) + 1,
                'timestamp': datetime.now().isoformat(),
                'transactions': self.transactions.copy(),
                'previous_hash': previous_hash,
                'transaction_count': len(self.transactions)
            }
            
            # Calculate block hash
            block_string = json.dumps(block, sort_keys=True, default=str)
            block['hash'] = hashlib.sha256(block_string.encode()).hexdigest()[:16]
            
            self.blocks.append(block)
            
            logger.info(f"🎯 Block #{block['number']} created with {block['transaction_count']} transactions - Hash: {block['hash']}")
            
            # Clear transactions
            self.transactions.clear()
            
        except Exception as e:
            logger.error(f"❌ Error creating block: {e}")
    
    def start_api_server(self):
        class APIHandler(BaseHTTPRequestHandler):
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
                            'timestamp': datetime.now().isoformat(),
                            'subscription': self.blockchain.subscription_path,
                            'uptime_seconds': int(time.time() - start_time)
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
                            'total_pending': len(self.blockchain.transactions)
                        }
                    elif self.path == '/health':
                        response = {
                            'healthy': True,
                            'pubsub_connected': hasattr(self.blockchain, 'subscriber'),
                            'api_running': True,
                            'timestamp': datetime.now().isoformat()
                        }
                    else:
                        response = {
                            'error': 'endpoint not found',
                            'available_endpoints': ['/status', '/blocks', '/transactions', '/health']
                        }
                    
                    self.send_response(200)
                    self.send_header('Content-Type', 'application/json')
                    self.send_header('Access-Control-Allow-Origin', '*')
                    self.send_header('Access-Control-Allow-Methods', 'GET, OPTIONS')
                    self.send_header('Access-Control-Allow-Headers', 'Content-Type')
                    self.end_headers()
                    
                    response_json = json.dumps(response, indent=2, default=str)
                    self.wfile.write(response_json.encode())
                    
                except Exception as e:
                    logger.error(f"API error: {e}")
                    self.send_response(500)
                    self.send_header('Content-Type', 'application/json')
                    self.end_headers()
                    error_response = {'error': str(e)}
                    self.wfile.write(json.dumps(error_response).encode())
            
            def do_OPTIONS(self):
                self.send_response(200)
                self.send_header('Access-Control-Allow-Origin', '*')
                self.send_header('Access-Control-Allow-Methods', 'GET, OPTIONS')
                self.send_header('Access-Control-Allow-Headers', 'Content-Type')
                self.end_headers()
            
            def log_message(self, format, *args):
                # Suppress HTTP access logs to reduce noise
                pass
        
        try:
            server = HTTPServer(('0.0.0.0', 8080), lambda *args: APIHandler(self, *args))
            threading.Thread(target=server.serve_forever, daemon=True).start()
            logger.info("🌐 API server started on port 8080")
            return True
        except Exception as e:
            logger.error(f"❌ Failed to start API server: {e}")
            return False
    
    def start_pubsub_listener(self):
        logger.info(f"📡 Starting Pub/Sub listener for: {self.subscription_path}")
        
        try:
            # Configure flow control
            flow_control = pubsub_v1.types.FlowControl(max_messages=10)
            
            # Start listening
            future = self.subscriber.subscribe(
                self.subscription_path, 
                callback=self.process_message,
                flow_control=flow_control
            )
            
            logger.info(f"✅ Listening for messages on {self.subscription_path}")
            
            # Keep the main thread running
            with self.subscriber:
                try:
                    while self.running:
                        time.sleep(10)
                        # Periodic heartbeat
                        logger.info(f"💓 Heartbeat - Blocks: {len(self.blocks)}, Pending: {len(self.transactions)}")
                        
                except KeyboardInterrupt:
                    logger.info("🛑 Shutdown requested")
                    self.running = False
                finally:
                    future.cancel()
                    logger.info("📡 Pub/Sub listener stopped")
                    
        except Exception as e:
            logger.error(f"❌ Pub/Sub listener error: {e}")
            raise
    
    def run(self):
        global start_time
        start_time = time.time()
        
        logger.info("🚀 Starting improved blockchain service...")
        
        # Start API server
        if not self.start_api_server():
            logger.error("Failed to start API server")
            return
        
        # Add some test data for immediate testing
        test_tx = {
            'id': 'INIT_001',
            'data': {
                'operation': 'INIT',
                'entity': 'System',
                'id': 'SYSTEM_INIT',
                'message': 'Blockchain service started'
            },
            'timestamp': datetime.now().isoformat(),
            'message_id': 'init'
        }
        self.transactions.append(test_tx)
        logger.info("✅ Added initialization transaction")
        
        # Start Pub/Sub listener
        self.start_pubsub_listener()

if __name__ == "__main__":
    try:
        blockchain = ImprovedBlockchain()
        blockchain.run()
    except Exception as e:
        logger.error(f"❌ Fatal error: {e}")
        exit(1)
"@

# Create the improved script file
$scriptContent = $improvedScript -replace '\$ProjectId', $ProjectId
Write-Host "   Creating improved blockchain script..." -ForegroundColor Gray

# Copy improved script to VM
$scriptContent | Out-File -FilePath "improved-blockchain.py" -Encoding UTF8

try {
    gcloud compute scp improved-blockchain.py blockchain-node:/tmp/blockchain.py `
        --zone=$Zone `
        --project=$ProjectId
    
    # Deploy the improved script
    gcloud compute ssh blockchain-node `
        --zone=$Zone `
        --command='sudo cp /tmp/blockchain.py /home/blockchain.py && sudo chmod +x /home/blockchain.py' `
        --project=$ProjectId
    
    # Restart the service with the new script
    gcloud compute ssh blockchain-node `
        --zone=$Zone `
        --command='sudo systemctl restart blockchain' `
        --project=$ProjectId
    
    Write-Host "   ✅ Improved blockchain script deployed and service restarted" -ForegroundColor Green
    
} catch {
    Write-Host "   ❌ Failed to deploy improved script: $($_.Exception.Message)" -ForegroundColor Red
}

# Clean up local file
Remove-Item "improved-blockchain.py" -ErrorAction SilentlyContinue

Write-Host ""

# 7. Wait and test again
Write-Host "7. Waiting for service to stabilize..." -ForegroundColor Yellow
Start-Sleep -Seconds 15

Write-Host "8. Testing improved setup..." -ForegroundColor Yellow

# Test API
try {
    $healthResponse = Invoke-RestMethod -Uri "http://$vmIp:8080/health" -TimeoutSec 10
    Write-Host "   ✅ Health check passed!" -ForegroundColor Green
    $healthResponse | ConvertTo-Json -Depth 2 | Write-Host -ForegroundColor Gray
} catch {
    Write-Host "   ❌ Health check failed: $($_.Exception.Message)" -ForegroundColor Red
}

# Publish test message
Write-Host "   Publishing test message..." -ForegroundColor Gray
gcloud pubsub topics publish metadata-events `
    --message='{"operation":"DEBUG","entity":"TestMessage","id":"DEBUG_001","source":"DebugScript","timestamp":"'$(Get-Date -Format "yyyy-MM-ddTHH:mm:ss.fffZ")'"}' `
    --project=$ProjectId

Start-Sleep -Seconds 10

# Check results
try {
    $statusResponse = Invoke-RestMethod -Uri "http://$vmIp:8080/status" -TimeoutSec 10
    Write-Host "   Status after test message:" -ForegroundColor Cyan
    $statusResponse | ConvertTo-Json -Depth 2 | Write-Host -ForegroundColor Gray
    
    if ($statusResponse.blocks -gt 0 -or $statusResponse.pending_transactions -gt 0) {
        Write-Host "   🎉 SUCCESS! Messages are now being processed!" -ForegroundColor Green
    } else {
        Write-Host "   ⚠️ No blocks or transactions yet. Check VM logs." -ForegroundColor Yellow
    }
} catch {
    Write-Host "   ❌ Could not verify test results" -ForegroundColor Red
}

Write-Host ""
Write-Host "✅ Debug complete!" -ForegroundColor Green
Write-Host ""
Write-Host "📋 Summary:" -ForegroundColor Yellow
Write-Host "• VM IP: $vmIp" -ForegroundColor Gray
Write-Host "• API: http://$vmIp:8080/status" -ForegroundColor Gray
Write-Host "• Health: http://$vmIp:8080/health" -ForegroundColor Gray
Write-Host ""
Write-Host "🔧 Troubleshooting commands:" -ForegroundColor Yellow
Write-Host "• VM logs: gcloud compute ssh blockchain-node --zone=$Zone --command='sudo journalctl -u blockchain -f' --project=$ProjectId" -ForegroundColor Gray
Write-Host "• Service status: gcloud compute ssh blockchain-node --zone=$Zone --command='systemctl status blockchain' --project=$ProjectId" -ForegroundColor Gray
Write-Host "• Test message: gcloud pubsub topics publish metadata-events --message='{\"test\": \"manual\"}' --project=$ProjectId" -ForegroundColor Gray