# test-simple-fixed.ps1 - Fixed version with proper error handling

param(
    [Parameter(Mandatory=$true)]
    [string]$ProjectId,
    [string]$Zone = "us-central1-a"
)

Write-Host "🧪 Testing GCP Blockchain (Fixed Version)" -ForegroundColor Green
Write-Host "Project: $ProjectId" -ForegroundColor Yellow
Write-Host ""

# Get VM IP with better error handling
Write-Host "Getting VM IP..." -ForegroundColor Yellow
try {
    $vmIp = gcloud compute instances describe blockchain-node `
        --zone=$Zone `
        --format="value(networkInterfaces[0].accessConfigs[0].natIP)" `
        --project=$ProjectId 2>$null
    
    if ([string]::IsNullOrEmpty($vmIp) -or $vmIp.Trim() -eq "") {
        throw "VM IP is empty or null"
    }
    
    $vmIp = $vmIp.Trim()
    Write-Host "VM IP: $vmIp" -ForegroundColor Cyan
} catch {
    Write-Host "❌ Could not get VM IP. Checking VM status..." -ForegroundColor Red
    
    try {
        $vmList = gcloud compute instances list --filter="name:blockchain-node" --project=$ProjectId
        Write-Host "VM List:" -ForegroundColor Yellow
        Write-Host $vmList -ForegroundColor Gray
        
        $vmStatus = gcloud compute instances describe blockchain-node `
            --zone=$Zone `
            --format="value(status)" `
            --project=$ProjectId 2>$null
        
        if ($vmStatus -eq "RUNNING") {
            Write-Host "VM is running but no external IP found. Checking network configuration..." -ForegroundColor Yellow
        } else {
            Write-Host "VM Status: $vmStatus" -ForegroundColor Red
            Write-Host "Starting VM..." -ForegroundColor Yellow
            gcloud compute instances start blockchain-node --zone=$Zone --project=$ProjectId
            Write-Host "Please wait a few minutes and try again." -ForegroundColor Yellow
        }
    } catch {
        Write-Host "❌ Error checking VM: $($_.Exception.Message)" -ForegroundColor Red
    }
    
    exit 1
}

Write-Host ""

# 1. Check if API is running
Write-Host "1. Checking API health..." -ForegroundColor Yellow
$apiUrl = "http://$vmIp`:8080"

try {
    # Try health endpoint first
    $healthResponse = Invoke-RestMethod -Uri "$apiUrl/health" -TimeoutSec 15
    Write-Host "✅ API Health check passed" -ForegroundColor Green
    Write-Host "Health Response:" -ForegroundColor Cyan
    $healthResponse | ConvertTo-Json -Depth 3 | Write-Host -ForegroundColor Gray
    
} catch {
    Write-Host "❌ Health check failed, trying status endpoint..." -ForegroundColor Yellow
    
    try {
        $statusResponse = Invoke-RestMethod -Uri "$apiUrl/status" -TimeoutSec 15
        Write-Host "✅ Status endpoint responding" -ForegroundColor Green
        Write-Host "Status Response:" -ForegroundColor Cyan
        $statusResponse | ConvertTo-Json -Depth 3 | Write-Host -ForegroundColor Gray
        
    } catch {
        Write-Host "❌ API not responding: $($_.Exception.Message)" -ForegroundColor Red
        Write-Host ""
        Write-Host "Troubleshooting steps:" -ForegroundColor Yellow
        Write-Host "1. Check if VM is running:" -ForegroundColor Gray
        Write-Host "   gcloud compute instances list --project=$ProjectId" -ForegroundColor Cyan
        Write-Host ""
        Write-Host "2. Check blockchain service status:" -ForegroundColor Gray
        Write-Host "   gcloud compute ssh blockchain-node --zone=$Zone --command='systemctl status blockchain' --project=$ProjectId" -ForegroundColor Cyan
        Write-Host ""
        Write-Host "3. Check service logs:" -ForegroundColor Gray
        Write-Host "   gcloud compute ssh blockchain-node --zone=$Zone --command='sudo journalctl -u blockchain -n 20' --project=$ProjectId" -ForegroundColor Cyan
        Write-Host ""
        Write-Host "4. Check firewall rules:" -ForegroundColor Gray
        Write-Host "   gcloud compute firewall-rules list --filter='name:allow-blockchain' --project=$ProjectId" -ForegroundColor Cyan
        Write-Host ""
        Write-Host "5. Run debug script:" -ForegroundColor Gray
        Write-Host "   .\debug-gcp-issues.ps1 -ProjectId $ProjectId" -ForegroundColor Cyan
        
        return
    }
}

Write-Host ""

# 2. Check initial blockchain state
Write-Host "2. Checking initial blockchain state..." -ForegroundColor Yellow
try {
    $initialStatus = Invoke-RestMethod -Uri "$apiUrl/status" -TimeoutSec 10
    Write-Host "Initial Status:" -ForegroundColor Cyan
    Write-Host "   Blocks: $($initialStatus.blocks)" -ForegroundColor Gray
    Write-Host "   Pending Transactions: $($initialStatus.pending_transactions)" -ForegroundColor Gray
    Write-Host "   Uptime: $($initialStatus.uptime_seconds) seconds" -ForegroundColor Gray
    
    $initialBlocks = Invoke-RestMethod -Uri "$apiUrl/blocks" -TimeoutSec 10
    Write-Host "   Total Blocks: $($initialBlocks.total_blocks)" -ForegroundColor Gray
    
} catch {
    Write-Host "⚠️ Could not get initial status: $($_.Exception.Message)" -ForegroundColor Yellow
}

Write-Host ""

# 3. Publish test messages with better tracking
Write-Host "3. Publishing test messages..." -ForegroundColor Yellow

$testMessages = @(
    @{ 
        operation = "CREATE"
        entity = "Customer"
        id = "CUST_TEST_001"
        source = "PowerShell_Test"
        description = "Test customer creation"
    },
    @{ 
        operation = "UPDATE"
        entity = "Order"
        id = "ORDER_TEST_123"
        source = "PowerShell_Test"
        description = "Test order update"
    },
    @{ 
        operation = "DELETE"
        entity = "Product"
        id = "PROD_TEST_456"
        source = "PowerShell_Test"
        description = "Test product deletion"
    }
)

$publishedMessages = @()

foreach ($i in 0..($testMessages.Count - 1)) {
    $msg = $testMessages[$i]
    $msg.timestamp = (Get-Date).ToString("yyyy-MM-ddTHH:mm:ss.fffZ")
    $msg.sequence = $i + 1
    $msg.test_session = "TEST_$(Get-Date -Format 'yyyyMMdd_HHmmss')"
    
    $jsonMessage = $msg | ConvertTo-Json -Compress
    
    Write-Host "   Publishing message $($i + 1): $($msg.operation) $($msg.entity) ($($msg.id))" -ForegroundColor Gray
    
    try {
        # Publish message and capture message ID
        $publishResult = gcloud pubsub topics publish metadata-events --message="$jsonMessage" --project=$ProjectId --format="value(messageIds[0])"
        $messageId = $publishResult.Trim()
        
        $publishedMessages += @{
            sequence = $i + 1
            messageId = $messageId
            entity = $msg.entity
            operation = $msg.operation
        }
        
        Write-Host "     ✅ Published with Message ID: $messageId" -ForegroundColor Green
        
    } catch {
        Write-Host "     ❌ Failed to publish message: $($_.Exception.Message)" -ForegroundColor Red
    }
    
    Start-Sleep -Seconds 2  # Space out messages
}

Write-Host ""
Write-Host "4. Waiting for message processing..." -ForegroundColor Yellow
Write-Host "   Waiting 15 seconds for Pub/Sub delivery and processing..." -ForegroundColor Gray
Start-Sleep -Seconds 15

# 5. Check updated status
Write-Host "5. Checking updated blockchain state..." -ForegroundColor Yellow
try {
    $updatedStatus = Invoke-RestMethod -Uri "$apiUrl/status" -TimeoutSec 10
    Write-Host "Updated Status:" -ForegroundColor Cyan
    Write-Host "   Blocks: $($updatedStatus.blocks)" -ForegroundColor Gray
    Write-Host "   Pending Transactions: $($updatedStatus.pending_transactions)" -ForegroundColor Gray
    
    # Check if blocks were created
    $updatedBlocks = Invoke-RestMethod -Uri "$apiUrl/blocks" -TimeoutSec 10
    Write-Host "   Total Blocks: $($updatedBlocks.total_blocks)" -ForegroundColor Gray
    
    if ($updatedBlocks.total_blocks -gt 0) {
        Write-Host "✅ Blockchain blocks created!" -ForegroundColor Green
        
        Write-Host "`nBlock details:" -ForegroundColor Cyan
        foreach ($block in $updatedBlocks.blocks) {
            Write-Host "   Block #$($block.number) - Hash: $($block.hash) - Transactions: $($block.transaction_count)" -ForegroundColor Gray
            
            # Show transaction details
            foreach ($tx in $block.transactions) {
                $data = $tx.data
                Write-Host "     → TX: $($tx.id) - $($data.operation) $($data.entity) ($($data.id))" -ForegroundColor Cyan
            }
        }
        
        # Show latest block in detail
        if ($updatedBlocks.blocks.Count -gt 0) {
            $latestBlock = $updatedBlocks.blocks[-1]
            Write-Host "`nLatest Block Details:" -ForegroundColor Yellow
            $latestBlock | ConvertTo-Json -Depth 5 | Write-Host -ForegroundColor Gray
        }
    } else {
        Write-Host "⚠️ No blocks created yet" -ForegroundColor Yellow
        
        # Check pending transactions
        $pendingTx = Invoke-RestMethod -Uri "$apiUrl/transactions" -TimeoutSec 10
        if ($pendingTx.total_pending -gt 0) {
            Write-Host "   Found $($pendingTx.total_pending) pending transactions:" -ForegroundColor Cyan
            foreach ($tx in $pendingTx.pending_transactions) {
                Write-Host "     → $($tx.id) - $($tx.data.operation) $($tx.data.entity)" -ForegroundColor Gray
            }
            Write-Host "   Transactions are being received but blocks aren't being created yet." -ForegroundColor Yellow
        } else {
            Write-Host "   No pending transactions found. Messages may not be reaching the service." -ForegroundColor Red
            Write-Host "   This suggests a Pub/Sub connectivity issue." -ForegroundColor Red
        }
    }
    
} catch {
    Write-Host "❌ Could not retrieve updated status: $($_.Exception.Message)" -ForegroundColor Red
}

Write-Host ""

# 6. Publish one more message to trigger block creation
if ($updatedBlocks.total_blocks -eq 0) {
    Write-Host "6. Publishing additional message to trigger block creation..." -ForegroundColor Yellow
    
    $triggerMessage = @{
        operation = "TRIGGER"
        entity = "BlockCreation"
        id = "TRIGGER_001"
        source = "Test_Script"
        timestamp = (Get-Date).ToString("yyyy-MM-ddTHH:mm:ss.fffZ")
        purpose = "Force block creation"
    } | ConvertTo-Json -Compress
    
    gcloud pubsub topics publish metadata-events --message="$triggerMessage" --project=$ProjectId
    
    Start-Sleep -Seconds 10
    
    try {
        $finalStatus = Invoke-RestMethod -Uri "$apiUrl/status" -TimeoutSec 10
        Write-Host "Final Status:" -ForegroundColor Cyan
        Write-Host "   Blocks: $($finalStatus.blocks)" -ForegroundColor Gray
        Write-Host "   Pending: $($finalStatus.pending_transactions)" -ForegroundColor Gray
    } catch {
        Write-Host "⚠️ Could not get final status" -ForegroundColor Yellow
    }
}

Write-Host ""
Write-Host "✅ Test complete!" -ForegroundColor Green
Write-Host ""

# Summary and next steps
Write-Host "📊 Test Summary:" -ForegroundColor Yellow
Write-Host "• Published $($publishedMessages.Count) test messages" -ForegroundColor Gray
Write-Host "• VM IP: $vmIp" -ForegroundColor Gray
Write-Host "• API Endpoint: $apiUrl" -ForegroundColor Gray

if ($updatedBlocks.total_blocks -gt 0) {
    Write-Host "• ✅ Blockchain is working correctly!" -ForegroundColor Green
    Write-Host "• Created $($updatedBlocks.total_blocks) blocks" -ForegroundColor Green
} else {
    Write-Host "• ⚠️ No blocks created - may need debugging" -ForegroundColor Yellow
    Write-Host "• Run: .\debug-gcp-issues.ps1 -ProjectId $ProjectId" -ForegroundColor Cyan
}

Write-Host ""
Write-Host "🔧 Useful commands:" -ForegroundColor Yellow
Write-Host "• Check status: Invoke-RestMethod -Uri $apiUrl/status" -ForegroundColor Gray
Write-Host "• View blocks: Invoke-RestMethod -Uri $apiUrl/blocks" -ForegroundColor Gray
Write-Host "• Health check: Invoke-RestMethod -Uri $apiUrl/health" -ForegroundColor Gray
Write-Host "• Publish test: gcloud pubsub topics publish metadata-events --message='{`"test`": `"manual`"}' --project=$ProjectId" -ForegroundColor Gray
Write-Host "• VM logs: gcloud compute ssh blockchain-node --zone=$Zone --command='sudo journalctl -u blockchain -f' --project=$ProjectId" -ForegroundColor Gray
Write-Host "• Debug: .\debug-gcp-issues.ps1 -ProjectId $ProjectId" -ForegroundColor Gray

Write-Host ""
Write-Host "🌐 Open in browser:" -ForegroundColor Yellow
Write-Host "• Status: $apiUrl/status" -ForegroundColor Cyan
Write-Host "• Health: $apiUrl/health" -ForegroundColor Cyan