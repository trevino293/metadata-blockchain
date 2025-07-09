# test-simple.ps1 - Test the simple GCP blockchain (Windows PowerShell)

param(
    [Parameter(Mandatory=$true)]
    [string]$ProjectId,
    [string]$Zone = "us-central1-a"
)

Write-Host "🧪 Testing GCP Blockchain" -ForegroundColor Green
Write-Host "Project: $ProjectId" -ForegroundColor Yellow
Write-Host ""

# Get VM IP
Write-Host "Getting VM IP..." -ForegroundColor Yellow
try {
    $vmIp = gcloud compute instances describe blockchain-node `
        --zone=$Zone `
        --format="value(networkInterfaces[0].accessConfigs[0].natIP)" `
        --project=$ProjectId
    
    if ([string]::IsNullOrEmpty($vmIp)) {
        throw "Could not get VM IP"
    }
    
    Write-Host "VM IP: $vmIp" -ForegroundColor Cyan
} catch {
    Write-Host "❌ Could not get VM IP. Is the VM running?" -ForegroundColor Red
    Write-Host "Check: gcloud compute instances list --project=$ProjectId" -ForegroundColor Gray
    exit 1
}

Write-Host ""

# 1. Check if API is running
Write-Host "1. Checking API..." -ForegroundColor Yellow
$apiUrl = "http://$vmIp:8080"

try {
    $response = Invoke-RestMethod -Uri "$apiUrl/status" -TimeoutSec 10
    Write-Host "✅ API is running" -ForegroundColor Green
    
    Write-Host "Status Response:" -ForegroundColor Cyan
    $response | ConvertTo-Json -Depth 3 | Write-Host -ForegroundColor Gray
    
} catch {
    Write-Host "❌ API not responding (may still be starting up)" -ForegroundColor Red
    Write-Host "Error: $($_.Exception.Message)" -ForegroundColor Red
    Write-Host ""
    Write-Host "Troubleshooting:" -ForegroundColor Yellow
    Write-Host "• Check if VM is running: gcloud compute instances list --project=$ProjectId" -ForegroundColor Gray
    Write-Host "• Check VM logs: gcloud compute ssh blockchain-node --zone=$Zone --command='sudo journalctl -u blockchain -n 20' --project=$ProjectId" -ForegroundColor Gray
    Write-Host "• Wait 2-3 minutes for service to start, then try again" -ForegroundColor Gray
    exit 1
}

Write-Host ""

# 2. Publish test messages
Write-Host "2. Publishing test messages..." -ForegroundColor Yellow

$testMessages = @(
    @{ operation = "CREATE"; entity = "Customer"; id = "CUST_001"; data = "New customer record" },
    @{ operation = "UPDATE"; entity = "Order"; id = "ORDER_123"; data = "Order status changed" },
    @{ operation = "DELETE"; entity = "Product"; id = "PROD_456"; data = "Product discontinued" },
    @{ operation = "CREATE"; entity = "Transaction"; id = "TXN_789"; data = "Payment processed" },
    @{ operation = "READ"; entity = "Report"; id = "RPT_001"; data = "Monthly report generated" }
)

foreach ($i in 0..($testMessages.Count - 1)) {
    $msg = $testMessages[$i]
    $msg.timestamp = (Get-Date).ToString("yyyy-MM-ddTHH:mm:ss.fffZ")
    $msg.sequence = $i + 1
    
    $jsonMessage = $msg | ConvertTo-Json -Compress
    
    Write-Host "   Publishing message $($i + 1): $($msg.operation) $($msg.entity)" -ForegroundColor Gray
    
    # Use gcloud to publish
    $escapedJson = $jsonMessage -replace '"', '\"'
    gcloud pubsub topics publish metadata-events --message="$jsonMessage" --project=$ProjectId
    
    Start-Sleep -Seconds 1
}

Write-Host ""
Write-Host "3. Waiting for processing..." -ForegroundColor Yellow
Start-Sleep -Seconds 15

# 3. Check status after publishing
Write-Host "4. Checking updated status..." -ForegroundColor Yellow
try {
    $statusResponse = Invoke-RestMethod -Uri "$apiUrl/status"
    Write-Host "Updated Status:" -ForegroundColor Cyan
    $statusResponse | ConvertTo-Json -Depth 3 | Write-Host -ForegroundColor Gray
} catch {
    Write-Host "⚠️ Could not get updated status" -ForegroundColor Yellow
}

Write-Host ""

# 4. Check blocks
Write-Host "5. Checking blockchain blocks..." -ForegroundColor Yellow
try {
    $blocksResponse = Invoke-RestMethod -Uri "$apiUrl/blocks"
    
    if ($blocksResponse.blocks.Count -gt 0) {
        Write-Host "✅ Blockchain blocks created!" -ForegroundColor Green
        Write-Host "Total blocks: $($blocksResponse.total_blocks)" -ForegroundColor Cyan
        
        Write-Host "`nBlock details:" -ForegroundColor Cyan
        foreach ($block in $blocksResponse.blocks) {
            Write-Host "  Block #$($block.number) - Hash: $($block.hash) - Transactions: $($block.transactions.Count)" -ForegroundColor Gray
        }
        
        # Show latest block details
        if ($blocksResponse.blocks.Count -gt 0) {
            $latestBlock = $blocksResponse.blocks[-1]
            Write-Host "`nLatest Block Details:" -ForegroundColor Cyan
            $latestBlock | ConvertTo-Json -Depth 4 | Write-Host -ForegroundColor Gray
        }
    } else {
        Write-Host "⚠️ No blocks created yet (messages may still be processing)" -ForegroundColor Yellow
    }
    
} catch {
    Write-Host "❌ Could not retrieve blocks: $($_.Exception.Message)" -ForegroundColor Red
}

Write-Host ""

# 5. Check pending transactions
Write-Host "6. Checking pending transactions..." -ForegroundColor Yellow
try {
    $transactionsResponse = Invoke-RestMethod -Uri "$apiUrl/transactions"
    
    if ($transactionsResponse.total_pending -gt 0) {
        Write-Host "Pending transactions: $($transactionsResponse.total_pending)" -ForegroundColor Cyan
        foreach ($tx in $transactionsResponse.pending_transactions) {
            Write-Host "  TX: $($tx.id) - $($tx.data.operation) $($tx.data.entity)" -ForegroundColor Gray
        }
    } else {
        Write-Host "No pending transactions" -ForegroundColor Gray
    }
} catch {
    Write-Host "⚠️ Could not get pending transactions" -ForegroundColor Yellow
}

Write-Host ""
Write-Host "✅ Test complete!" -ForegroundColor Green
Write-Host ""
Write-Host "🔧 Useful commands:" -ForegroundColor Yellow
Write-Host "• Check status: Invoke-RestMethod -Uri http://$vmIp:8080/status" -ForegroundColor Gray
Write-Host "• View blocks: Invoke-RestMethod -Uri http://$vmIp:8080/blocks" -ForegroundColor Gray
Write-Host "• Publish message: gcloud pubsub topics publish metadata-events --message='{`"test`": `"message`"}' --project=$ProjectId" -ForegroundColor Gray
Write-Host "• View VM logs: gcloud compute ssh blockchain-node --zone=$Zone --command='sudo journalctl -u blockchain -f' --project=$ProjectId" -ForegroundColor Gray
Write-Host "• Cleanup: .\cleanup-simple.ps1 $ProjectId" -ForegroundColor Gray

Write-Host ""
Write-Host "🌐 Web interface: http://$vmIp:8080/status" -ForegroundColor Cyan

# Create a simple HTML file to view in browser
$htmlContent = @"
<!DOCTYPE html>
<html>
<head>
    <title>GCP Blockchain Status</title>
    <style>
        body { font-family: Arial, sans-serif; margin: 20px; }
        .status { background: #f0f8ff; padding: 15px; border-radius: 5px; margin: 10px 0; }
        .block { background: #f5f5f5; padding: 10px; margin: 5px 0; border-left: 4px solid #007cba; }
        button { background: #007cba; color: white; border: none; padding: 10px 15px; margin: 5px; cursor: pointer; }
        pre { background: #f9f9f9; padding: 10px; overflow: auto; }
    </style>
</head>
<body>
    <h1>GCP Blockchain Monitor</h1>
    <p>API Endpoint: <a href="http://$vmIp:8080/status" target="_blank">http://$vmIp:8080/status</a></p>
    
    <button onclick="loadStatus()">Refresh Status</button>
    <button onclick="loadBlocks()">Load Blocks</button>
    <button onclick="publishTest()">Publish Test Message</button>
    
    <div id="content"></div>
    
    <script>
        async function loadStatus() {
            try {
                const response = await fetch('http://$vmIp:8080/status');
                const data = await response.json();
                document.getElementById('content').innerHTML = 
                    '<div class="status"><h3>Status</h3><pre>' + JSON.stringify(data, null, 2) + '</pre></div>';
            } catch (e) {
                document.getElementById('content').innerHTML = '<p>Error loading status: ' + e.message + '</p>';
            }
        }
        
        async function loadBlocks() {
            try {
                const response = await fetch('http://$vmIp:8080/blocks');
                const data = await response.json();
                let html = '<div class="status"><h3>Blocks</h3>';
                data.blocks.forEach(block => {
                    html += '<div class="block"><strong>Block #' + block.number + '</strong> - Hash: ' + block.hash + '<br>';
                    html += 'Transactions: ' + block.transactions.length + ' - Time: ' + block.timestamp + '</div>';
                });
                html += '</div>';
                document.getElementById('content').innerHTML = html;
            } catch (e) {
                document.getElementById('content').innerHTML = '<p>Error loading blocks: ' + e.message + '</p>';
            }
        }
        
        function publishTest() {
            alert('Use gcloud command to publish:\ngcloud pubsub topics publish metadata-events --message=\'{"test": "browser"}\' --project=$ProjectId');
        }
        
        // Auto-load status on page load
        loadStatus();
    </script>
</body>
</html>
"@

$htmlContent | Out-File -FilePath "blockchain-monitor.html" -Encoding UTF8
Write-Host "📄 HTML monitor saved to: blockchain-monitor.html" -ForegroundColor Yellow
Write-Host "   Open this file in your browser to monitor the blockchain" -ForegroundColor Grayt=$PROJECT_ID"