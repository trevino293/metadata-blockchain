# debug-gcp-issues-fixed.ps1 - Fixed version with proper URL formatting

param(
    [Parameter(Mandatory=$true)]
    [string]$ProjectId,
    [string]$Zone = "us-central1-a"
)

Write-Host "🔧 Debugging GCP Blockchain Issues (FIXED)" -ForegroundColor Yellow
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
    
    # Trim whitespace from IP
    $vmIp = $vmIp.Trim()
    
    Write-Host "   VM Status: $vmStatus" -ForegroundColor Green
    Write-Host "   VM IP: $vmIp" -ForegroundColor Green
    
    # Validate IP format
    if ($vmIp -match '^\d+\.\d+\.\d+\.\d+$') {
        Write-Host "   ✅ Valid IP format" -ForegroundColor Green
    } else {
        Write-Host "   ❌ Invalid IP format: '$vmIp'" -ForegroundColor Red
        exit 1
    }
    
    if ($vmStatus -ne "RUNNING") {
        Write-Host "❌ VM is not running! Starting it..." -ForegroundColor Red
        gcloud compute instances start blockchain-node --zone=$Zone --project=$ProjectId
        Write-Host "⏳ Waiting for VM to start..." -ForegroundColor Yellow
        Start-Sleep -Seconds 30
        
        # Get IP again after starting
        $vmIp = gcloud compute instances describe blockchain-node `
            --zone=$Zone `
            --format="value(networkInterfaces[0].accessConfigs[0].natIP)" `
            --project=$ProjectId
        $vmIp = $vmIp.Trim()
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

# 3. Test API with proper URL formatting
Write-Host "3. Testing API..." -ForegroundColor Yellow

# Build URLs properly
$apiBaseUrl = "http://$vmIp" + ":8080"
$statusUrl = "$apiBaseUrl/status"
$healthUrl = "$apiBaseUrl/health"

Write-Host "   API Base URL: $apiBaseUrl" -ForegroundColor Cyan
Write-Host "   Status URL: $statusUrl" -ForegroundColor Cyan
Write-Host "   Health URL: $healthUrl" -ForegroundColor Cyan

try {
    Write-Host "   Testing health endpoint..." -ForegroundColor Gray
    $healthResponse = Invoke-RestMethod -Uri $healthUrl -TimeoutSec 15
    Write-Host "   ✅ Health check passed!" -ForegroundColor Green
    Write-Host "   Response: $($healthResponse | ConvertTo-Json -Compress)" -ForegroundColor Cyan
    
} catch {
    Write-Host "   ❌ Health check failed: $($_.Exception.Message)" -ForegroundColor Yellow
    
    try {
        Write-Host "   Trying status endpoint..." -ForegroundColor Gray
        $statusResponse = Invoke-RestMethod -Uri $statusUrl -TimeoutSec 15
        Write-Host "   ✅ Status endpoint responding!" -ForegroundColor Green
        Write-Host "   Status: $($statusResponse.status)" -ForegroundColor Cyan
        Write-Host "   Blocks: $($statusResponse.blocks)" -ForegroundColor Cyan
        Write-Host "   Pending: $($statusResponse.pending_transactions)" -ForegroundColor Cyan
        
    } catch {
        Write-Host "   ❌ API not responding: $($_.Exception.Message)" -ForegroundColor Red
        
        # Check if it's a connectivity issue
        Write-Host "   Testing basic connectivity..." -ForegroundColor Gray
        try {
            $ping = Test-NetConnection -ComputerName $vmIp -Port 8080 -WarningAction SilentlyContinue
            if ($ping.TcpTestSucceeded) {
                Write-Host "   ✅ Port 8080 is reachable" -ForegroundColor Green
                Write-Host "   ❓ Service may be starting up or misconfigured" -ForegroundColor Yellow
            } else {
                Write-Host "   ❌ Port 8080 is not reachable" -ForegroundColor Red
                Write-Host "   🔍 Checking firewall rules..." -ForegroundColor Yellow
                
                $firewallRule = gcloud compute firewall-rules describe allow-blockchain --project=$ProjectId 2>$null
                if ($LASTEXITCODE -eq 0) {
                    Write-Host "   ✅ Firewall rule exists" -ForegroundColor Green
                } else {
                    Write-Host "   ❌ Firewall rule missing!" -ForegroundColor Red
                    Write-Host "   Creating firewall rule..." -ForegroundColor Yellow
                    gcloud compute firewall-rules create allow-blockchain --allow=tcp:8080 --source-ranges=0.0.0.0/0 --target-tags=blockchain --project=$ProjectId
                }
            }
        } catch {
            Write-Host "   ❌ Network test failed: $($_.Exception.Message)" -ForegroundColor Red
        }
    }
}

Write-Host ""

# 4. Check VM service logs
Write-Host "4. Checking VM service status..." -ForegroundColor Yellow
try {
    Write-Host "   Checking service status..." -ForegroundColor Gray
    $serviceStatus = gcloud compute ssh blockchain-node `
        --zone=$Zone `
        --command='systemctl is-active blockchain' `
        --project=$ProjectId 2>$null
    
    Write-Host "   Service status: $serviceStatus" -ForegroundColor Cyan
    
    Write-Host "   Getting recent logs..." -ForegroundColor Gray
    $vmLogs = gcloud compute ssh blockchain-node `
        --zone=$Zone `
        --command='sudo journalctl -u blockchain --no-pager -n 10' `
        --project=$ProjectId 2>$null
    
    if ($vmLogs) {
        Write-Host "   Recent logs:" -ForegroundColor Cyan
        $vmLogs | ForEach-Object { Write-Host "     $_" -ForegroundColor Gray }
    } else {
        Write-Host "   ⚠️ Could not fetch logs" -ForegroundColor Yellow
    }
    
    if ($serviceStatus -ne "active") {
        Write-Host "   ❌ Service not active! Restarting..." -ForegroundColor Red
        
        gcloud compute ssh blockchain-node `
            --zone=$Zone `
            --command='sudo systemctl restart blockchain' `
            --project=$ProjectId
        
        Write-Host "   Waiting for service to restart..." -ForegroundColor Yellow
        Start-Sleep -Seconds 15
        
        $newStatus = gcloud compute ssh blockchain-node `
            --zone=$Zone `
            --command='systemctl is-active blockchain' `
            --project=$ProjectId 2>$null
        
        Write-Host "   New service status: $newStatus" -ForegroundColor Cyan
    }
    
} catch {
    Write-Host "   ❌ Could not check service: $($_.Exception.Message)" -ForegroundColor Red
}

Write-Host ""

# 5. Test with a simple message
Write-Host "5. Testing with a simple message..." -ForegroundColor Yellow

$testMessage = @{
    operation = "DEBUG_TEST"
    entity = "TestEntity"
    id = "DEBUG_001"
    source = "FixedDebugScript"
    timestamp = (Get-Date).ToString("yyyy-MM-ddTHH:mm:ss.fffK")
} | ConvertTo-Json -Compress

Write-Host "   Publishing test message..." -ForegroundColor Gray
Write-Host "   Message: $testMessage" -ForegroundColor Cyan

try {
    $publishResult = gcloud pubsub topics publish metadata-events --message="$testMessage" --project=$ProjectId
    Write-Host "   ✅ Message published successfully" -ForegroundColor Green
    Write-Host "   Result: $publishResult" -ForegroundColor Gray
    
    Write-Host "   Waiting 10 seconds for processing..." -ForegroundColor Yellow
    Start-Sleep -Seconds 10
    
    # Check if the message was processed
    try {
        $finalStatus = Invoke-RestMethod -Uri $statusUrl -TimeoutSec 10
        Write-Host "   Final status:" -ForegroundColor Cyan
        Write-Host "     Blocks: $($finalStatus.blocks)" -ForegroundColor Gray
        Write-Host "     Pending: $($finalStatus.pending_transactions)" -ForegroundColor Gray
        Write-Host "     Messages processed: $($finalStatus.total_messages_processed)" -ForegroundColor Gray
        
        if ($finalStatus.blocks -gt 0 -or $finalStatus.pending_transactions -gt 0) {
            Write-Host "   🎉 SUCCESS! System is processing messages!" -ForegroundColor Green
        } else {
            Write-Host "   ⚠️ No activity detected after message" -ForegroundColor Yellow
        }
        
    } catch {
        Write-Host "   ❌ Could not verify message processing: $($_.Exception.Message)" -ForegroundColor Red
    }
    
} catch {
    Write-Host "   ❌ Failed to publish test message: $($_.Exception.Message)" -ForegroundColor Red
}

Write-Host ""

# 6. Summary and recommendations
Write-Host "🎯 Summary and Recommendations:" -ForegroundColor Green
Write-Host ""
Write-Host "📋 Connection Details:" -ForegroundColor Yellow
Write-Host "   • VM IP: $vmIp" -ForegroundColor Gray
Write-Host "   • API Base: $apiBaseUrl" -ForegroundColor Gray
Write-Host "   • Status: $statusUrl" -ForegroundColor Gray
Write-Host "   • Health: $healthUrl" -ForegroundColor Gray
Write-Host ""

# Test the API one more time to give final verdict
try {
    $finalTest = Invoke-RestMethod -Uri $healthUrl -TimeoutSec 5
    Write-Host "✅ FINAL RESULT: API is working correctly!" -ForegroundColor Green
    Write-Host ""
    Write-Host "🚀 Quick test commands:" -ForegroundColor Yellow
    Write-Host "   Invoke-RestMethod -Uri '$statusUrl'" -ForegroundColor Cyan
    Write-Host "   Invoke-RestMethod -Uri '$healthUrl'" -ForegroundColor Cyan
    Write-Host ""
    Write-Host "📊 Current state:" -ForegroundColor Yellow
    Write-Host "   • Status: $($finalTest.status)" -ForegroundColor Gray
    Write-Host "   • Uptime: $($finalTest.uptime_seconds) seconds" -ForegroundColor Gray
    
} catch {
    Write-Host "❌ FINAL RESULT: API is still not responding" -ForegroundColor Red
    Write-Host ""
    Write-Host "🔧 Troubleshooting next steps:" -ForegroundColor Yellow
    Write-Host "1. Check if service is actually running:" -ForegroundColor Cyan
    Write-Host "   gcloud compute ssh blockchain-node --zone=$Zone --command='sudo systemctl status blockchain' --project=$ProjectId" -ForegroundColor Gray
    Write-Host ""
    Write-Host "2. Check detailed logs:" -ForegroundColor Cyan
    Write-Host "   gcloud compute ssh blockchain-node --zone=$Zone --command='sudo journalctl -u blockchain -f' --project=$ProjectId" -ForegroundColor Gray
    Write-Host ""
    Write-Host "3. Test internal connectivity:" -ForegroundColor Cyan
    Write-Host "   gcloud compute ssh blockchain-node --zone=$Zone --command='curl -v localhost:8080/health' --project=$ProjectId" -ForegroundColor Gray
    Write-Host ""
    Write-Host "4. Restart everything:" -ForegroundColor Cyan
    Write-Host "   gcloud compute instances reset blockchain-node --zone=$Zone --project=$ProjectId" -ForegroundColor Gray
    Write-Host ""
    Write-Host "5. If all else fails, redeploy:" -ForegroundColor Cyan
    Write-Host "   .\cleanup-simple.ps1 -ProjectId $ProjectId -Force" -ForegroundColor Gray
    Write-Host "   .\simple-gcp-deploy.ps1 -ProjectId $ProjectId" -ForegroundColor Gray
}

Write-Host ""
Write-Host "💡 Tips for using the system:" -ForegroundColor Yellow
Write-Host "• Use the fixed URLs shown above" -ForegroundColor Gray
Write-Host "• Wait 5-10 minutes after VM startup for full initialization" -ForegroundColor Gray
Write-Host "• Check VM logs if messages aren't being processed" -ForegroundColor Gray
Write-Host "• Use 'gcloud compute instances reset' if the service won't start" -ForegroundColor Gray