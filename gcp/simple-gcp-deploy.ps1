# simple-gcp-deploy-easy-scopes.ps1 - Version with simplified scopes

param(
    [Parameter(Mandatory=$true)]
    [string]$ProjectId,
    [string]$Region = "us-central1",
    [string]$Zone = "us-central1-a",
    [string]$VmName = "blockchain-node",
    [string]$MachineType = "e2-small",
    [switch]$Force,
    [switch]$SkipCleanup
)

Write-Host "🚀 Simple GCP Blockchain Deployment (Easy Scopes)" -ForegroundColor Green
Write-Host "Project: $ProjectId" -ForegroundColor Yellow
Write-Host "Zone: $Zone" -ForegroundColor Yellow
Write-Host "VM Name: $VmName" -ForegroundColor Yellow
Write-Host ""

# Function to check if resource exists
function Test-GcpResource {
    param(
        [string]$ResourceType,
        [string]$ResourceName,
        [string]$ProjectId,
        [string]$Zone = $null
    )
    
    try {
        switch ($ResourceType) {
            "vm" {
                $result = gcloud compute instances describe $ResourceName --zone=$Zone --project=$ProjectId --format="value(name)" 2>$null
            }
            "firewall" {
                $result = gcloud compute firewall-rules describe $ResourceName --project=$ProjectId --format="value(name)" 2>$null
            }
            "topic" {
                $result = gcloud pubsub topics describe $ResourceName --project=$ProjectId --format="value(name)" 2>$null
            }
            "subscription" {
                $result = gcloud pubsub subscriptions describe $ResourceName --project=$ProjectId --format="value(name)" 2>$null
            }
        }
        return (-not [string]::IsNullOrEmpty($result))
    } catch {
        return $false
    }
}

# Function to safely delete resource
function Remove-GcpResource {
    param(
        [string]$ResourceType,
        [string]$ResourceName,
        [string]$ProjectId,
        [string]$Zone = $null
    )
    
    if (Test-GcpResource -ResourceType $ResourceType -ResourceName $ResourceName -ProjectId $ProjectId -Zone $Zone) {
        Write-Host "   Removing existing $ResourceType`: $ResourceName..." -ForegroundColor Yellow
        
        try {
            switch ($ResourceType) {
                "vm" {
                    gcloud compute instances delete $ResourceName --zone=$Zone --project=$ProjectId --quiet 2>$null
                }
                "firewall" {
                    gcloud compute firewall-rules delete $ResourceName --project=$ProjectId --quiet 2>$null
                }
                "topic" {
                    gcloud pubsub topics delete $ResourceName --project=$ProjectId --quiet 2>$null
                }
                "subscription" {
                    gcloud pubsub subscriptions delete $ResourceName --project=$ProjectId --quiet 2>$null
                }
            }
            Write-Host "   ✅ Removed $ResourceName" -ForegroundColor Green
        } catch {
            Write-Host "   ⚠️ Could not remove $ResourceName (may not exist)" -ForegroundColor Yellow
        }
    }
}

# Cleanup existing resources if Force is specified
if ($Force -and -not $SkipCleanup) {
    Write-Host "🧹 Cleaning up existing resources..." -ForegroundColor Yellow
    Remove-GcpResource -ResourceType "vm" -ResourceName $VmName -ProjectId $ProjectId -Zone $Zone
    Remove-GcpResource -ResourceType "firewall" -ResourceName "allow-blockchain" -ProjectId $ProjectId
    Remove-GcpResource -ResourceType "subscription" -ResourceName "blockchain-sub" -ProjectId $ProjectId
    Remove-GcpResource -ResourceType "topic" -ResourceName "metadata-events" -ProjectId $ProjectId
    Write-Host ""
}

# 1. Enable required APIs
Write-Host "1. Enabling required APIs..." -ForegroundColor Yellow
try {
    gcloud services enable compute.googleapis.com pubsub.googleapis.com logging.googleapis.com --project=$ProjectId
    Write-Host "   ✅ APIs enabled" -ForegroundColor Green
} catch {
    Write-Host "   ⚠️ Some APIs may already be enabled" -ForegroundColor Yellow
}

Write-Host ""

# 2. Create Pub/Sub resources with error handling
Write-Host "2. Setting up Pub/Sub..." -ForegroundColor Yellow

# Create topic
if (-not (Test-GcpResource -ResourceType "topic" -ResourceName "metadata-events" -ProjectId $ProjectId)) {
    try {
        gcloud pubsub topics create metadata-events --project=$ProjectId
        Write-Host "   ✅ Topic 'metadata-events' created" -ForegroundColor Green
    } catch {
        Write-Host "   ❌ Failed to create topic" -ForegroundColor Red
        exit 1
    }
} else {
    Write-Host "   ✅ Topic 'metadata-events' already exists" -ForegroundColor Green
}

# Create subscription
if (-not (Test-GcpResource -ResourceType "subscription" -ResourceName "blockchain-sub" -ProjectId $ProjectId)) {
    try {
        gcloud pubsub subscriptions create blockchain-sub --topic=metadata-events --project=$ProjectId
        Write-Host "   ✅ Subscription 'blockchain-sub' created" -ForegroundColor Green
    } catch {
        Write-Host "   ❌ Failed to create subscription" -ForegroundColor Red
        exit 1
    }
} else {
    Write-Host "   ✅ Subscription 'blockchain-sub' already exists" -ForegroundColor Green
}

Write-Host ""

# 3. Create simplified startup script
Write-Host "3. Creating startup script..." -ForegroundColor Yellow

$startupScript = @"
#!/bin/bash
echo "=== Blockchain Node Setup ==="
date

# Update system
apt-get update -y
apt-get install -y python3 python3-pip curl jq

# Install Python packages
pip3 install google-cloud-pubsub

# Create simple blockchain service
cat > /home/blockchain.py << 'EOFPY'
#!/usr/bin/env python3
import json
import time
import hashlib
import logging
import threading
from datetime import datetime
from google.cloud import pubsub_v1
from http.server import HTTPServer, BaseHTTPRequestHandler

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

class SimpleBlockchain:
    def __init__(self):
        self.project_id = "$ProjectId"
        self.subscription_path = f"projects/{self.project_id}/subscriptions/blockchain-sub"
        self.blocks = []
        self.transactions = []
        self.message_count = 0
        self.start_time = time.time()
        
        try:
            self.subscriber = pubsub_v1.SubscriberClient()
            logger.info(f"Blockchain initialized for project: {self.project_id}")
        except Exception as e:
            logger.error(f"Failed to initialize: {e}")
            raise
    
    def process_message(self, message):
        self.message_count += 1
        
        try:
            data = json.loads(message.data.decode('utf-8'))
            
            tx = {
                'id': hashlib.sha256(f"{time.time()}{json.dumps(data)}".encode()).hexdigest()[:16],
                'data': data,
                'timestamp': datetime.now().isoformat(),
                'sequence': self.message_count
            }
            
            self.transactions.append(tx)
            logger.info(f"Transaction {tx['id']}: {data.get('operation', 'UNKNOWN')}")
            
            # Create block with 2+ transactions
            if len(self.transactions) >= 2:
                self.create_block()
            
            message.ack()
            
        except Exception as e:
            logger.error(f"Error processing message: {e}")
            message.ack()  # Acknowledge to prevent redelivery
    
    def create_block(self):
        if not self.transactions:
            return
            
        block = {
            'number': len(self.blocks) + 1,
            'timestamp': datetime.now().isoformat(),
            'transactions': self.transactions.copy(),
            'transaction_count': len(self.transactions)
        }
        
        block['hash'] = hashlib.sha256(json.dumps(block, sort_keys=True).encode()).hexdigest()[:16]
        self.blocks.append(block)
        
        logger.info(f"BLOCK #{block['number']} created with {block['transaction_count']} transactions")
        self.transactions.clear()
    
    def start_api(self):
        class Handler(BaseHTTPRequestHandler):
            def __init__(self, blockchain, *args, **kwargs):
                self.blockchain = blockchain
                super().__init__(*args, **kwargs)
            
            def do_GET(self):
                if self.path == '/status':
                    response = {
                        'status': 'running',
                        'blocks': len(self.blockchain.blocks),
                        'pending': len(self.blockchain.transactions),
                        'messages': self.blockchain.message_count,
                        'uptime': int(time.time() - self.blockchain.start_time)
                    }
                elif self.path == '/health':
                    response = {'healthy': True, 'api_running': True}
                elif self.path == '/blocks':
                    response = {'blocks': self.blockchain.blocks[-5:], 'total': len(self.blockchain.blocks)}
                else:
                    response = {'error': 'Not found', 'endpoints': ['/status', '/health', '/blocks']}
                
                self.send_response(200)
                self.send_header('Content-Type', 'application/json')
                self.send_header('Access-Control-Allow-Origin', '*')
                self.end_headers()
                self.wfile.write(json.dumps(response, indent=2).encode())
            
            def log_message(self, format, *args): pass
        
        server = HTTPServer(('0.0.0.0', 8080), lambda *args: Handler(self, *args))
        threading.Thread(target=server.serve_forever, daemon=True).start()
        logger.info("API server started on port 8080")
    
    def run(self):
        logger.info("Starting blockchain service...")
        self.start_api()
        
        with self.subscriber:
            future = self.subscriber.subscribe(self.subscription_path, callback=self.process_message)
            logger.info(f"Listening to {self.subscription_path}")
            
            try:
                while True:
                    time.sleep(30)
                    logger.info(f"Status: {len(self.blocks)} blocks, {len(self.transactions)} pending")
            except KeyboardInterrupt:
                future.cancel()

if __name__ == "__main__":
    blockchain = SimpleBlockchain()
    blockchain.run()
EOFPY

chmod +x /home/blockchain.py

# Create systemd service
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
Environment=GOOGLE_CLOUD_PROJECT=$ProjectId

[Install]
WantedBy=multi-user.target
EOFSVC

systemctl daemon-reload
systemctl enable blockchain
systemctl start blockchain

echo "Blockchain service setup complete"
systemctl status blockchain --no-pager
"@

$finalStartupScript = $startupScript -replace '\$ProjectId', $ProjectId
$finalStartupScript | Out-File -FilePath "startup-script.txt" -Encoding UTF8

Write-Host "   ✅ Startup script created" -ForegroundColor Green
Write-Host ""

# 4. Create VM with cloud-platform scope (simplest approach)
Write-Host "4. Creating VM with simplified permissions..." -ForegroundColor Yellow

if (Test-GcpResource -ResourceType "vm" -ResourceName $VmName -ProjectId $ProjectId -Zone $Zone) {
    if (-not $Force) {
        Write-Host "   ❌ VM '$VmName' already exists! Use -Force to recreate." -ForegroundColor Red
        exit 1
    } else {
        Remove-GcpResource -ResourceType "vm" -ResourceName $VmName -ProjectId $ProjectId -Zone $Zone
        Start-Sleep -Seconds 10
    }
}

try {
    # Option 1: Use cloud-platform scope (broadest, but simplest)
    Write-Host "   Attempting with cloud-platform scope..." -ForegroundColor Gray
    
    gcloud compute instances create $VmName `
        --zone=$Zone `
        --machine-type=$MachineType `
        --image-family=ubuntu-2204-lts `
        --image-project=ubuntu-os-cloud `
        --boot-disk-size=200GB `
        --boot-disk-type=pd-standard `
        --metadata-from-file=startup-script=startup-script.txt `
        --tags=blockchain `
        --scopes=cloud-platform `
        --project=$ProjectId
    
    Write-Host "   ✅ VM '$VmName' created successfully" -ForegroundColor Green
    
} catch {
    Write-Host "   ⚠️ cloud-platform scope failed, trying specific scopes..." -ForegroundColor Yellow
    
    try {
        # Option 2: Try with specific scopes (properly formatted)
        gcloud compute instances create $VmName `
            --zone=$Zone `
            --machine-type=$MachineType `
            --image-family=ubuntu-2204-lts `
            --image-project=ubuntu-os-cloud `
            --boot-disk-size=200GB `
            --boot-disk-type=pd-standard `
            --metadata-from-file=startup-script=startup-script.txt `
            --tags=blockchain `
            --scopes="pubsub,logging-write" `
            --project=$ProjectId
        
        Write-Host "   ✅ VM '$VmName' created with specific scopes" -ForegroundColor Green
        
    } catch {
        Write-Host "   ⚠️ Specific scopes failed, trying default scopes..." -ForegroundColor Yellow
        
        try {
            # Option 3: Use default scopes
            gcloud compute instances create $VmName `
                --zone=$Zone `
                --machine-type=$MachineType `
                --image-family=ubuntu-2204-lts `
                --image-project=ubuntu-os-cloud `
                --boot-disk-size=200GB `
                --boot-disk-type=pd-standard `
                --metadata-from-file=startup-script=startup-script.txt `
                --tags=blockchain `
                --project=$ProjectId
            
            Write-Host "   ✅ VM '$VmName' created with default scopes" -ForegroundColor Green
            Write-Host "   ⚠️ Note: May need to manually enable service account permissions" -ForegroundColor Yellow
            
        } catch {
            Write-Host "   ❌ All attempts failed: $($_.Exception.Message)" -ForegroundColor Red
            Write-Host ""
            Write-Host "Troubleshooting suggestions:" -ForegroundColor Yellow
            Write-Host "1. Check available images: gcloud compute images list --filter='family:ubuntu-22' --project=ubuntu-os-cloud" -ForegroundColor Cyan
            Write-Host "2. Check project permissions: gcloud projects get-iam-policy $ProjectId" -ForegroundColor Cyan
            Write-Host "3. Try different zone: --zone=us-west1-a" -ForegroundColor Cyan
            Write-Host "4. Check quotas: gcloud compute project-info describe --project=$ProjectId" -ForegroundColor Cyan
            exit 1
        }
    }
}

Write-Host ""

# 5. Create firewall rule
Write-Host "5. Setting up firewall..." -ForegroundColor Yellow

if (-not (Test-GcpResource -ResourceType "firewall" -ResourceName "allow-blockchain" -ProjectId $ProjectId)) {
    try {
        gcloud compute firewall-rules create allow-blockchain `
            --allow=tcp:8080 `
            --source-ranges=0.0.0.0/0 `
            --target-tags=blockchain `
            --project=$ProjectId
        
        Write-Host "   ✅ Firewall rule created" -ForegroundColor Green
    } catch {
        Write-Host "   ❌ Failed to create firewall rule" -ForegroundColor Red
        exit 1
    }
} else {
    Write-Host "   ✅ Firewall rule already exists" -ForegroundColor Green
}

Write-Host ""

# 6. Get VM IP and test
Write-Host "6. Getting VM IP and testing..." -ForegroundColor Yellow

$maxWait = 120
$elapsed = 0
$vmIp = $null

while ($elapsed -lt $maxWait -and $null -eq $vmIp) {
    Start-Sleep -Seconds 5
    $elapsed += 5
    
    try {
        $ipResult = gcloud compute instances describe $VmName `
            --zone=$Zone `
            --format="value(networkInterfaces[0].accessConfigs[0].natIP)" `
            --project=$ProjectId 2>$null
        
        if (-not [string]::IsNullOrWhiteSpace($ipResult)) {
            $vmIp = $ipResult.Trim()
            if ($vmIp -match '^\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3}$') {
                Write-Host "   ✅ VM IP: $vmIp" -ForegroundColor Green
                break
            } else {
                $vmIp = $null
            }
        }
    } catch {
        # Continue waiting
    }
}

if ($null -eq $vmIp) {
    Write-Host "   ❌ Could not get VM IP" -ForegroundColor Red
    exit 1
}

# Test API
Write-Host "   Testing API (waiting for service to start)..." -ForegroundColor Gray
$apiUrl = "http://$vmIp`:8080"
$serviceReady = $false
$testAttempts = 0

while (-not $serviceReady -and $testAttempts -lt 10) {
    $testAttempts++
    Start-Sleep -Seconds 10
    
    try {
        $response = Invoke-RestMethod -Uri "$apiUrl/health" -TimeoutSec 5
        if ($response.healthy) {
            $serviceReady = $true
            Write-Host "   ✅ Service is healthy!" -ForegroundColor Green
        }
    } catch {
        Write-Host "   Attempt $testAttempts/10..." -ForegroundColor Gray
    }
}

# Cleanup
if (-not $SkipCleanup) {
    Remove-Item "startup-script.txt" -ErrorAction SilentlyContinue
}

# Save connection info
@"
# Simple GCP Blockchain Deployment
VM IP: $vmIp
API Status: $apiUrl/status
API Health: $apiUrl/health
API Blocks: $apiUrl/blocks

# Test commands:
Invoke-RestMethod -Uri "$apiUrl/status"
gcloud pubsub topics publish metadata-events --message='{"test":"message"}' --project=$ProjectId

# SSH to VM:
gcloud compute ssh $VmName --zone=$Zone --project=$ProjectId

# Clean up:
gcloud compute instances delete $VmName --zone=$Zone --project=$ProjectId --quiet
gcloud compute firewall-rules delete allow-blockchain --project=$ProjectId --quiet
gcloud pubsub subscriptions delete blockchain-sub --project=$ProjectId --quiet
gcloud pubsub topics delete metadata-events --project=$ProjectId --quiet
"@ | Out-File -FilePath "connection-info.txt" -Encoding UTF8

Write-Host ""
Write-Host "✅ Deployment Complete!" -ForegroundColor Green
Write-Host "• VM IP: $vmIp" -ForegroundColor Cyan
Write-Host "• API: $apiUrl" -ForegroundColor Cyan

if ($serviceReady) {
    Write-Host "• Service: ✅ Ready" -ForegroundColor Green
} else {
    Write-Host "• Service: ⚠️ Starting (check logs)" -ForegroundColor Yellow
    Write-Host "  gcloud compute ssh $VmName --zone=$Zone --command='sudo journalctl -u blockchain -f' --project=$ProjectId" -ForegroundColor Cyan
}

Write-Host ""
Write-Host "🧪 Quick test:" -ForegroundColor Yellow
Write-Host "Invoke-RestMethod -Uri '$apiUrl/status'" -ForegroundColor Cyan
Write-Host "gcloud pubsub topics publish metadata-events --message='{`"operation`":`"TEST`"}' --project=$ProjectId" -ForegroundColor Cyan