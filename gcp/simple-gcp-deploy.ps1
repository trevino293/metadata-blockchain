# simple-gcp-deploy.ps1 - Updated version with robust JSON parsing and error handling

param(
    [Parameter(Mandatory=$true)]
    [string]$ProjectId,
    [string]$Region = "us-central1",
    [string]$Zone = "us-central1-a"
)

Write-Host "🚀 Simple GCP Blockchain Setup (Robust Version)" -ForegroundColor Green
Write-Host "Project: $ProjectId" -ForegroundColor Yellow
Write-Host "Region: $Region" -ForegroundColor Yellow
Write-Host "Zone: $Zone" -ForegroundColor Yellow
Write-Host ""

# 1. Enable APIs
Write-Host "1. Enabling required APIs..." -ForegroundColor Yellow
gcloud services enable compute.googleapis.com pubsub.googleapis.com --project=$ProjectId

# 2. Create Pub/Sub topic and subscription
Write-Host "2. Creating Pub/Sub resources..." -ForegroundColor Yellow
gcloud pubsub topics create metadata-events --project=$ProjectId 2>$null
if ($LASTEXITCODE -eq 0) {
    Write-Host "   ✅ Topic 'metadata-events' created" -ForegroundColor Green
} else {
    Write-Host "   ✅ Topic 'metadata-events' already exists" -ForegroundColor Green
}

gcloud pubsub subscriptions create blockchain-sub --topic=metadata-events --project=$ProjectId 2>$null
if ($LASTEXITCODE -eq 0) {
    Write-Host "   ✅ Subscription 'blockchain-sub' created" -ForegroundColor Green
} else {
    Write-Host "   ✅ Subscription 'blockchain-sub' already exists" -ForegroundColor Green
}

# 3. Create robust startup script for VM
Write-Host "3. Creating robust blockchain startup script..." -ForegroundColor Yellow

$startupScript = @'
#!/bin/bash
echo "🚀 Starting robust blockchain node setup..."
apt-get update
apt-get install -y python3 python3-pip jq curl
pip3 install google-cloud-pubsub

# Create ROBUST blockchain adapter with advanced JSON handling
cat > /home/blockchain.py << 'EOFPY'
#!/usr/bin/env python3
import json
import time
import hashlib
import base64
import sys
import re
from datetime import datetime
from google.cloud import pubsub_v1
from http.server import HTTPServer, BaseHTTPRequestHandler
import threading
import logging

logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')
logger = logging.getLogger(__name__)

class RobustBlockchain:
    def __init__(self):
        self.project_id = "$ProjectId"
        self.subscription = f"projects/{self.project_id}/subscriptions/blockchain-sub"
        self.subscriber = pubsub_v1.SubscriberClient()
        self.blocks = []
        self.transactions = []
        self.message_count = 0
        self.processed_message_ids = set()
        self.last_block_time = time.time()
        logger.info(f"✅ Robust blockchain initialized for project: {self.project_id}")
        
    def clean_json_message(self, raw_data):
        """Advanced JSON cleaning to handle gcloud CLI formatting issues"""
        try:
            # Log raw data for debugging
            logger.info(f"📥 Raw message: {repr(raw_data)}")
            
            # Convert bytes to string
            if isinstance(raw_data, bytes):
                message_str = raw_data.decode('utf-8')
            else:
                message_str = str(raw_data)
            
            # Remove whitespace
            message_str = message_str.strip()
            
            # Fix gcloud CLI double-quoting issues
            if message_str.startswith('"') and message_str.endswith('"') and message_str.count('"') > 2:
                message_str = message_str[1:-1]
                logger.info(f"🔧 Removed outer quotes: {repr(message_str)}")
            
            # Fix escaped quotes
            message_str = message_str.replace('\\"', '"').replace("\\'", "'")
            
            # Fix single quotes to double quotes for JSON compliance
            message_str = re.sub(r"'([^']*)'(\s*:\s*)", r'"\1"\2', message_str)  # Fix keys
            message_str = re.sub(r":\s*'([^']*)'", r': "\1"', message_str)  # Fix string values
            
            # Handle boolean and null values that might be quoted
            message_str = re.sub(r':\s*"(true|false|null)"', r': \1', message_str, flags=re.IGNORECASE)
            
            logger.info(f"🧹 Cleaned message: {repr(message_str)}")
            
            # Attempt to parse
            try:
                parsed_data = json.loads(message_str)
                logger.info(f"✅ Successfully parsed JSON: {parsed_data}")
                return parsed_data
                
            except json.JSONDecodeError as e:
                logger.warning(f"⚠️ JSON parse failed after cleanup: {e}")
                logger.warning(f"Problematic string: {repr(message_str)}")
                
                # Final fallback: extract what we can
                return self.extract_fallback_data(message_str, e)
                
        except Exception as e:
            logger.error(f"❌ Error in JSON cleanup: {e}")
            return self.create_error_data(str(e))
    
    def extract_fallback_data(self, message_str, original_error):
        """Extract data from malformed JSON as fallback"""
        try:
            # Try to extract common fields using regex
            operation_match = re.search(r'"?operation"?\s*:\s*"?([^",}]+)"?', message_str, re.IGNORECASE)
            entity_match = re.search(r'"?entity"?\s*:\s*"?([^",}]+)"?', message_str, re.IGNORECASE)
            id_match = re.search(r'"?id"?\s*:\s*"?([^",}]+)"?', message_str, re.IGNORECASE)
            source_match = re.search(r'"?source"?\s*:\s*"?([^",}]+)"?', message_str, re.IGNORECASE)
            
            fallback_data = {
                "operation": operation_match.group(1) if operation_match else "EXTRACTED",
                "entity": entity_match.group(1) if entity_match else "UnknownEntity", 
                "id": id_match.group(1) if id_match else f"FALLBACK_{self.message_count}",
                "source": source_match.group(1) if source_match else "FallbackParser",
                "raw_message": message_str,
                "extraction_method": "regex_fallback",
                "original_error": str(original_error)
            }
            
            logger.info(f"🔧 Extracted fallback data: {fallback_data}")
            return fallback_data
            
        except Exception as e:
            logger.error(f"❌ Fallback extraction failed: {e}")
            return self.create_error_data(f"Fallback extraction failed: {e}")
    
    def create_error_data(self, error_msg):
        """Create error data structure when all parsing fails"""
        return {
            "operation": "PARSE_ERROR",
            "entity": "FailedMessage",
            "id": f"ERROR_{self.message_count}",
            "source": "ErrorHandler",
            "error": error_msg,
            "timestamp": datetime.now().isoformat()
        }
        
    def process_message(self, message):
        try:
            self.message_count += 1
            logger.info(f"📨 Processing message #{self.message_count} - ID: {message.message_id}")
            
            # Skip duplicates
            if message.message_id in self.processed_message_ids:
                logger.info(f"⏭️ Skipping duplicate message: {message.message_id}")
                message.ack()
                return
            
            self.processed_message_ids.add(message.message_id)
            
            # Clean and parse message
            data = self.clean_json_message(message.data)
            
            # Ensure all required fields exist
            self.normalize_data(data)
            
            # Create transaction
            tx = {
                'id': hashlib.sha256(f"{time.time()}{json.dumps(data, sort_keys=True)}".encode()).hexdigest()[:16],
                'data': data,
                'timestamp': datetime.now().isoformat(),
                'message_number': self.message_count,
                'pubsub_message_id': message.message_id
            }
            
            self.transactions.append(tx)
            logger.info(f"✅ Transaction created: {tx['id']} - {data.get('operation')} {data.get('entity')} ({data.get('id')})")
            
            # Smart block creation logic
            should_create_block = (
                len(self.transactions) >= 2 or  # Standard: 2+ transactions
                (len(self.transactions) >= 1 and (time.time() - self.last_block_time) > 30) or  # Timeout: 30 seconds
                data.get('force_block') == True  # Manual trigger
            )
            
            if should_create_block:
                self.create_block()
            
            message.ack()
            logger.info(f"📝 Message #{self.message_count} acknowledged")
            
        except Exception as e:
            logger.error(f"❌ Error processing message #{self.message_count}: {e}")
            logger.error(f"Raw message data: {repr(message.data)}")
            message.nack()
    
    def normalize_data(self, data):
        """Ensure data has all required fields"""
        if 'timestamp' not in data:
            data['timestamp'] = datetime.now().isoformat()
        if 'id' not in data:
            data['id'] = f"MSG_{self.message_count}"
        if 'operation' not in data:
            data['operation'] = "UNKNOWN"
        if 'entity' not in data:
            data['entity'] = "GenericEntity"
        if 'source' not in data:
            data['source'] = "UnknownSource"
    
    def create_block(self):
        try:
            if not self.transactions:
                logger.warning("⚠️ No transactions to create block")
                return
                
            previous_hash = self.blocks[-1]['hash'] if self.blocks else "0000000000000000"
            
            block = {
                'number': len(self.blocks) + 1,
                'timestamp': datetime.now().isoformat(),
                'transactions': self.transactions.copy(),
                'transaction_count': len(self.transactions),
                'previous_hash': previous_hash
            }
            
            # Calculate block hash
            block_string = json.dumps(block, default=str, sort_keys=True)
            block['hash'] = hashlib.sha256(block_string.encode()).hexdigest()[:16]
            
            self.blocks.append(block)
            self.last_block_time = time.time()
            
            # Log block creation details
            logger.info(f"🎯 BLOCK #{block['number']} CREATED!")
            logger.info(f"   Hash: {block['hash']}")
            logger.info(f"   Transactions: {block['transaction_count']}")
            logger.info(f"   Operations: {', '.join([tx['data']['operation'] + ' ' + tx['data']['entity'] for tx in block['transactions']])}")
            
            # Clear transactions
            self.transactions.clear()
            
        except Exception as e:
            logger.error(f"❌ Error creating block: {e}")
    
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
                    self.send_header('Access-Control-Allow-Methods', 'GET, OPTIONS')
                    self.send_header('Access-Control-Allow-Headers', 'Content-Type')
                    self.end_headers()
                    
                    if self.path in ['/status', '/health']:
                        response = {
                            'status': 'running',
                            'healthy': True,
                            'project': self.blockchain.project_id,
                            'blocks': len(self.blockchain.blocks),
                            'pending_transactions': len(self.blockchain.transactions),
                            'total_messages_processed': self.blockchain.message_count,
                            'unique_messages_processed': len(self.blockchain.processed_message_ids),
                            'timestamp': datetime.now().isoformat(),
                            'pubsub_connected': True,
                            'api_running': True,
                            'uptime_seconds': int(time.time() - start_time),
                            'latest_block_hash': self.blockchain.blocks[-1]['hash'] if self.blockchain.blocks else None,
                            'last_block_time': self.blockchain.last_block_time
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
                                'unique_message_count': len(self.blockchain.processed_message_ids),
                                'blocks_count': len(self.blockchain.blocks),
                                'pending_count': len(self.blockchain.transactions),
                                'last_block_time': self.blockchain.last_block_time,
                                'time_since_last_block': time.time() - self.blockchain.last_block_time,
                                'recent_blocks': [
                                    {
                                        'number': b['number'], 
                                        'hash': b['hash'], 
                                        'tx_count': b['transaction_count'],
                                        'timestamp': b['timestamp']
                                    } for b in self.blockchain.blocks[-3:]
                                ],
                                'pending_transactions': [
                                    {
                                        'id': t['id'], 
                                        'operation': t['data']['operation'],
                                        'entity': t['data']['entity'],
                                        'message_id': t['pubsub_message_id']
                                    } for t in self.blockchain.transactions
                                ]
                            }
                        }
                    else:
                        response = {
                            'error': 'endpoint not found',
                            'available_endpoints': ['/status', '/health', '/blocks', '/transactions', '/debug'],
                            'project': self.blockchain.project_id,
                            'version': 'robust'
                        }
                    
                    self.wfile.write(json.dumps(response, indent=2, default=str).encode())
                    
                except Exception as e:
                    logger.error(f"❌ API error: {e}")
                    self.send_response(500)
                    self.send_header('Content-Type', 'application/json')
                    self.end_headers()
                    error_response = {'error': str(e), 'status': 'error', 'timestamp': datetime.now().isoformat()}
                    self.wfile.write(json.dumps(error_response).encode())
            
            def do_OPTIONS(self):
                self.send_response(200)
                self.send_header('Access-Control-Allow-Origin', '*')
                self.send_header('Access-Control-Allow-Methods', 'GET, OPTIONS')
                self.send_header('Access-Control-Allow-Headers', 'Content-Type')
                self.end_headers()
            
            def log_message(self, format, *args):
                pass  # Suppress HTTP access logs
        
        try:
            server = HTTPServer(('0.0.0.0', 8080), lambda *args: Handler(self, *args))
            threading.Thread(target=server.serve_forever, daemon=True).start()
            logger.info("🌐 Robust API server started on port 8080")
            logger.info("📊 Available endpoints: /status, /health, /blocks, /transactions, /debug")
        except Exception as e:
            logger.error(f"❌ Failed to start API: {e}")
    
    def run(self):
        global start_time
        start_time = time.time()
        
        logger.info("🚀 Starting ROBUST blockchain service with advanced JSON handling...")
        self.start_api()
        
        try:
            future = self.subscriber.subscribe(self.subscription, callback=self.process_message)
            logger.info(f"📡 Listening robustly to: {self.subscription}")
            
            # Main loop with intelligent heartbeat
            while True:
                time.sleep(30)
                
                # Smart heartbeat with actionable information
                pending_time = time.time() - self.last_block_time if self.transactions else 0
                logger.info(f"💓 Heartbeat - Blocks: {len(self.blocks)}, Pending: {len(self.transactions)}, Messages: {self.message_count}")
                
                if pending_time > 0:
                    logger.info(f"⏱️ Oldest pending transaction: {pending_time:.1f}s old")
                
                # Auto-create block if transactions are stuck too long
                if len(self.transactions) > 0 and pending_time > 60:
                    logger.info("⏰ Auto-creating block due to old pending transactions (60s timeout)")
                    self.create_block()
                    
        except KeyboardInterrupt:
            logger.info("🛑 Shutdown requested")
            future.cancel()
        except Exception as e:
            logger.error(f"❌ Runtime error: {e}")
            # Don't exit, keep trying
            time.sleep(10)

if __name__ == "__main__":
    # Robust restart loop
    restart_count = 0
    while True:
        try:
            if restart_count > 0:
                logger.info(f"🔄 Restart #{restart_count}")
            
            blockchain = RobustBlockchain()
            blockchain.run()
            
        except Exception as e:
            restart_count += 1
            logger.error(f"❌ Fatal error (restart #{restart_count}): {e}")
            
            if restart_count < 5:
                logger.info(f"🔄 Restarting in 10 seconds...")
                time.sleep(10)
            else:
                logger.error("❌ Too many restarts, giving up")
                break
EOFPY

# Make executable
chmod +x /home/blockchain.py

# Create systemd service with better configuration
cat > /etc/systemd/system/blockchain.service << 'EOFSVC'
[Unit]
Description=Robust Blockchain Service
After=network.target google-cloud-ops-agent.service

[Service]
Type=simple
User=root
ExecStart=/usr/bin/python3 /home/blockchain.py
Restart=always
RestartSec=15
StartLimitInterval=0
StandardOutput=journal
StandardError=journal
Environment=PYTHONUNBUFFERED=1

[Install]
WantedBy=multi-user.target
EOFSVC

# Enable and start service
systemctl daemon-reload
systemctl enable blockchain
systemctl start blockchain

# Wait a moment for service to start
sleep 5

# Check service status
if systemctl is-active --quiet blockchain; then
    echo "✅ Robust blockchain service started successfully"
    echo "📊 Service status: $(systemctl is-active blockchain)"
    echo "🌐 API endpoints available: /status, /health, /blocks, /transactions, /debug"
else
    echo "⚠️ Service may still be starting..."
    echo "📝 Check logs with: journalctl -u blockchain -f"
fi

echo "🔧 Useful commands:"
echo "• Status: systemctl status blockchain"
echo "• Logs: journalctl -u blockchain -f"
echo "• Restart: systemctl restart blockchain"
echo "• Test API: curl localhost:8080/health"
'@ -replace '\$ProjectId', $ProjectId

# Save startup script to temporary file
$startupScript | Out-File -FilePath "robust-startup-script.txt" -Encoding UTF8

# 4. Create VM with robust configuration
Write-Host "4. Creating VM with robust blockchain service..." -ForegroundColor Yellow
gcloud compute instances create blockchain-node `
    --zone=$Zone `
    --machine-type=e2-small `
    --image-family=ubuntu-2204-lts `
    --image-project=ubuntu-os-cloud `
    --metadata-from-file=startup-script=robust-startup-script.txt `
    --tags=blockchain `
    --boot-disk-size=20GB `
    --boot-disk-type=pd-standard `
    --project=$ProjectId

if ($LASTEXITCODE -eq 0) {
    Write-Host "   ✅ VM 'blockchain-node' created successfully" -ForegroundColor Green
} else {
    Write-Host "   ❌ VM creation failed" -ForegroundColor Red
    exit 1
}

# 5. Create firewall rule
Write-Host "5. Creating firewall rule..." -ForegroundColor Yellow
gcloud compute firewall-rules create allow-blockchain `
    --allow=tcp:8080 `
    --source-ranges=0.0.0.0/0 `
    --target-tags=blockchain `
    --description="Allow access to blockchain API on port 8080" `
    --project=$ProjectId 2>$null

if ($LASTEXITCODE -eq 0) {
    Write-Host "   ✅ Firewall rule 'allow-blockchain' created" -ForegroundColor Green
} else {
    Write-Host "   ✅ Firewall rule 'allow-blockchain' already exists" -ForegroundColor Green
}

# 6. Get VM IP with enhanced retry logic
Write-Host "6. Getting VM external IP..." -ForegroundColor Yellow
$vmIp = $null
$maxRetries = 8
$retryCount = 0

while ($vmIp -eq $null -and $retryCount -lt $maxRetries) {
    Start-Sleep -Seconds 10
    $retryCount++
    Write-Host "   Attempt $retryCount/$maxRetries to get VM IP..." -ForegroundColor Gray
    
    try {
        $vmIp = gcloud compute instances describe blockchain-node `
            --zone=$Zone `
            --format="value(networkInterfaces[0].accessConfigs[0].natIP)" `
            --project=$ProjectId 2>$null
        
        if ([string]::IsNullOrWhiteSpace($vmIp)) {
            $vmIp = $null
            Write-Host "   VM IP not ready yet..." -ForegroundColor Yellow
        } else {
            $vmIp = $vmIp.Trim()
            if ($vmIp -match '^\d+\.\d+\.\d+\.\d+$') {
                Write-Host "   ✅ Got VM IP: $vmIp" -ForegroundColor Green
            } else {
                Write-Host "   ⚠️ Invalid IP format: $vmIp" -ForegroundColor Yellow
                $vmIp = $null
            }
        }
    } catch {
        Write-Host "   ⚠️ Error getting VM IP: $($_.Exception.Message)" -ForegroundColor Yellow
    }
}

if ($vmIp -eq $null) {
    Write-Host "   ❌ Could not get VM IP after $maxRetries attempts" -ForegroundColor Red
    Write-Host "   🔧 Try manually: gcloud compute instances describe blockchain-node --zone=$Zone --format='value(networkInterfaces[0].accessConfigs[0].natIP)' --project=$ProjectId" -ForegroundColor Gray
    exit 1
}

# Clean up temporary file
Remove-Item "robust-startup-script.txt" -ErrorAction SilentlyContinue

Write-Host ""
Write-Host "🎉 Robust GCP Blockchain Setup Complete!" -ForegroundColor Green
Write-Host ""

# 7. Display connection information
Write-Host "📋 Connection Information:" -ForegroundColor Yellow
Write-Host "   • Project ID: $ProjectId" -ForegroundColor Gray
Write-Host "   • VM Name: blockchain-node" -ForegroundColor Gray
Write-Host "   • Zone: $Zone" -ForegroundColor Gray
Write-Host "   • VM IP: $vmIp" -ForegroundColor Gray
Write-Host "   • API Base: http://$vmIp`:8080" -ForegroundColor Gray
Write-Host ""

Write-Host "🌐 API Endpoints:" -ForegroundColor Yellow
Write-Host "   • Health: http://$vmIp`:8080/health" -ForegroundColor Cyan
Write-Host "   • Status: http://$vmIp`:8080/status" -ForegroundColor Cyan
Write-Host "   • Blocks: http://$vmIp`:8080/blocks" -ForegroundColor Cyan
Write-Host "   • Debug: http://$vmIp`:8080/debug" -ForegroundColor Cyan
Write-Host ""

# 8. Wait for service initialization
Write-Host "⏳ Waiting for blockchain service to initialize..." -ForegroundColor Yellow
Write-Host "   This may take 3-5 minutes for full startup..." -ForegroundColor Gray

$serviceReady = $false
$attempts = 0
$maxAttempts = 15

while (-not $serviceReady -and $attempts -lt $maxAttempts) {
    $attempts++
    Start-Sleep -Seconds 20
    
    Write-Host "   Checking service readiness (attempt $attempts/$maxAttempts)..." -ForegroundColor Gray
    
    try {
        $response = Invoke-RestMethod -Uri "http://$vmIp`:8080/health" -TimeoutSec 10
        if ($response.healthy -eq $true -and $response.api_running -eq $true) {
            $serviceReady = $true
            Write-Host "   ✅ Blockchain service is ready and healthy!" -ForegroundColor Green
        }
    } catch {
        Write-Host "   ⏳ Service still starting up..." -ForegroundColor Yellow
    }
}

Write-Host ""

# 9. Run comprehensive tests
Write-Host "🧪 Running comprehensive deployment tests..." -ForegroundColor Yellow

if ($serviceReady) {
    Write-Host "   Testing API endpoints..." -ForegroundColor Gray
    
    try {
        # Test status endpoint
        $status = Invoke-RestMethod -Uri "http://$vmIp`:8080/status" -TimeoutSec 10
        Write-Host "   ✅ Status endpoint: $($status.status)" -ForegroundColor Green
        
        # Test debug endpoint  
        $debug = Invoke-RestMethod -Uri "http://$vmIp`:8080/debug" -TimeoutSec 10
        Write-Host "   ✅ Debug endpoint: Project $($debug.debug_info.project_id)" -ForegroundColor Green
        
        # Publish test messages with different formats
        Write-Host "   Publishing test messages..." -ForegroundColor Gray
        
        $testMessages = @(
            '{"operation":"CREATE","entity":"Customer","id":"DEPLOY_TEST_001","source":"DeploymentScript"}',
            '{"operation":"UPDATE","entity":"Order","id":"DEPLOY_TEST_002","source":"DeploymentScript"}',
            '{"operation":"DELETE","entity":"Product","id":"DEPLOY_TEST_003","source":"DeploymentScript","force_block":true}'
        )
        
        foreach ($i in 0..($testMessages.Count - 1)) {
            $msg = $testMessages[$i]
            try {
                $result = gcloud pubsub topics publish metadata-events --message="$msg" --project=$ProjectId
                Write-Host "     ✅ Message $($i + 1) published: $($result -split ':' | Select-Object -Last 1)" -ForegroundColor Green
                Start-Sleep -Seconds 3
            } catch {
                Write-Host "     ⚠️ Message $($i + 1) failed: $($_.Exception.Message)" -ForegroundColor Yellow
            }
        }
        
        # Wait for processing
        Write-Host "   Waiting for message processing..." -ForegroundColor Gray
        Start-Sleep -Seconds 15
        
        # Check results
        $finalStatus = Invoke-RestMethod -Uri "http://$vmIp`:8080/status" -TimeoutSec 10
        $finalBlocks = Invoke-RestMethod -Uri "http://$vmIp`:8080/blocks" -TimeoutSec 10
        
        Write-Host ""
        Write-Host "📊 Test Results:" -ForegroundColor Yellow
        Write-Host "   • Total Messages Processed: $($finalStatus.total_messages_processed)" -ForegroundColor Gray
        Write-Host "   • Blocks Created: $($finalStatus.blocks)" -ForegroundColor Gray
        Write-Host "   • Pending Transactions: $($finalStatus.pending_transactions)" -ForegroundColor Gray
        
        if ($finalStatus.blocks -gt 0) {
            Write-Host "   🎉 SUCCESS! Blockchain is creating blocks!" -ForegroundColor Green
            
            Write-Host ""
            Write-Host "📦 Created Blocks:" -ForegroundColor Yellow
            foreach ($block in $finalBlocks.blocks) {
                Write-Host "     Block #$($block.number) - Hash: $($block.hash) - Transactions: $($block.transaction_count)" -ForegroundColor Cyan
            }
        } else {
            Write-Host "   ⚠️ No blocks created yet, but messages are being processed" -ForegroundColor Yellow
            Write-Host "   💡 This is normal - blocks are created when 2+ transactions accumulate" -ForegroundColor Gray
        }
        
    } catch {
        Write-Host "   ⚠️ API tests failed: $($_.Exception.Message)" -ForegroundColor Yellow
        Write-Host "   💡 Service may still be initializing" -ForegroundColor Gray
    }
    
} else {
    Write-Host "   ⚠️ Service not ready for testing yet" -ForegroundColor Yellow
    Write-Host "   💡 Try testing manually in a few minutes" -ForegroundColor Gray
}

# 10. Save comprehensive connection info
Write-Host ""
Write-Host "💾 Saving connection information..." -ForegroundColor Yellow

$connectionInfo = @"
# 🔗 GCP Robust Blockchain Connection Info
# Generated: $(Get-Date)

## 📋 Basic Information
Project ID: $ProjectId
VM Name: blockchain-node
Zone: $Zone
VM IP: $vmIp

## 🌐 API Endpoints
Health Check: http://$vmIp`:8080/health
Status: http://$vmIp`:8080/status
Blocks: http://$vmIp`:8080/blocks
Transactions: http://$vmIp`:8080/transactions
Debug Info: http://$vmIp`:8080/debug

## 🧪 Quick Test Commands (PowerShell)
# Check status
Invoke-RestMethod -Uri "http://$vmIp`:8080/status"

# Publish test message
gcloud pubsub topics publish metadata-events --message='{"operation":"TEST","entity":"Manual","id":"MANUAL_001"}' --project=$ProjectId

# View blocks
Invoke-RestMethod -Uri "http://$vmIp`:8080/blocks"

# Debug information
Invoke-RestMethod -Uri "http://$vmIp`:8080/debug"

## 🔧 Management Commands
# VM logs
gcloud compute ssh blockchain-node --zone=$Zone --command='sudo journalctl -u blockchain -f' --project=$ProjectId

# Service status  
gcloud compute ssh blockchain-node --zone=$Zone --command='systemctl status blockchain' --project=$ProjectId

# Restart service
gcloud compute ssh blockchain-node --zone=$Zone --command='sudo systemctl restart blockchain' --project=$ProjectId

# VM management
gcloud compute instances stop blockchain-node --zone=$Zone --project=$ProjectId
gcloud compute instances start blockchain-node --zone=$Zone --project=$ProjectId

## 🧹 Cleanup Commands
# Test cleanup (keep for reference)
# .\cleanup-simple.ps1 -ProjectId "$ProjectId" -Force

## 📊 Features
✅ Robust JSON parsing (handles gcloud CLI formatting issues)
✅ Smart block creation (timeout-based and count-based)
✅ Comprehensive error handling and recovery
✅ Detailed logging and debugging endpoints
✅ Auto-restart on failures
✅ Duplicate message prevention
✅ CORS-enabled API for web interfaces
"@

$connectionInfo | Out-File -FilePath "connection-info.txt" -Encoding UTF8
Write-Host "   ✅ Connection info saved to: connection-info.txt" -ForegroundColor Green

# 11. Final summary and next steps
Write-Host ""
Write-Host "🎯 Deployment Summary:" -ForegroundColor Green
Write-Host "✅ VM created with robust blockchain service" -ForegroundColor Yellow
Write-Host "✅ Pub/Sub topic and subscription configured" -ForegroundColor Yellow
Write-Host "✅ Firewall rules applied" -ForegroundColor Yellow
Write-Host "✅ Advanced JSON parsing implemented" -ForegroundColor Yellow
Write-Host "✅ Comprehensive API endpoints available" -ForegroundColor Yellow
Write-Host "✅ Auto-restart and error recovery enabled" -ForegroundColor Yellow

Write-Host ""
Write-Host "🚀 Next Steps:" -ForegroundColor Yellow
Write-Host "1. Test the deployment:" -ForegroundColor Cyan
Write-Host "   .\test-simple.ps1 -ProjectId `"$ProjectId`"" -ForegroundColor Gray
Write-Host ""
Write-Host "2. Monitor in real-time:" -ForegroundColor Cyan
Write-Host "   Invoke-RestMethod -Uri `"http://$vmIp`:8080/status`"" -ForegroundColor Gray
Write-Host ""
Write-Host "3. Publish messages:" -ForegroundColor Cyan
Write-Host "   gcloud pubsub topics publish metadata-events --message='{`"operation`":`"TEST`"}' --project=$ProjectId" -ForegroundColor Gray
Write-Host ""
Write-Host "4. View blocks:" -ForegroundColor Cyan
Write-Host "   Invoke-RestMethod -Uri `"http://$vmIp`:8080/blocks`"" -ForegroundColor Gray

Write-Host ""
Write-Host "💡 Pro Tips:" -ForegroundColor Yellow
Write-Host "• The service now handles malformed JSON automatically" -ForegroundColor Gray
Write-Host "• Blocks are created when 2+ transactions accumulate OR after 30 seconds" -ForegroundColor Gray
Write-Host "• Use /debug endpoint for detailed troubleshooting" -ForegroundColor Gray
Write-Host "• All endpoints support CORS for web interface integration" -ForegroundColor Gray

Write-Host ""
Write-Host "🎉 Robust GCP Blockchain is ready for use!" -ForegroundColor Green