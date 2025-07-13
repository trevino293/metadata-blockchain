# gcp-peer-test.ps1
# Comprehensive test script for GCP Fabric peer with Pub/Sub integration

param(
    [Parameter(Mandatory=$true)]
    [string]$ProjectId,
    
    [string]$VmName = "fabric-peer",
    [string]$Zone = "us-central1-a",
    [string]$Topic = "blockchain-metadata",
    [string]$ChannelName = "metadata-channel",
    [switch]$ContinuousMonitoring
)

Write-Host "🧪 Comprehensive GCP Fabric Peer Testing" -ForegroundColor Green
Write-Host "========================================" -ForegroundColor Green
Write-Host "Testing: Fabric P2P + Pub/Sub Integration" -ForegroundColor Yellow
Write-Host ""

# Initialize test results
$testResults = @{
    TotalTests = 0
    Passed = 0
    Failed = 0
    Warnings = 0
}

function Test-Component {
    param(
        [string]$Name,
        [scriptblock]$Test,
        [string]$SuccessMessage,
        [string]$FailureMessage
    )
    
    $testResults.TotalTests++
    Write-Host "🔍 Testing: $Name" -ForegroundColor Yellow
    
    try {
        $result = & $Test
        if ($result) {
            Write-Host "   ✅ $SuccessMessage" -ForegroundColor Green
            $testResults.Passed++
            return $true
        } else {
            Write-Host "   ❌ $FailureMessage" -ForegroundColor Red
            $testResults.Failed++
            return $false
        }
    } catch {
        Write-Host "   ❌ Error: $_" -ForegroundColor Red
        $testResults.Failed++
        return $false
    }
}

# Get VM information
Write-Host "📋 Getting VM information..." -ForegroundColor Yellow
try {
    $vmInfo = gcloud compute instances describe $VmName --zone=$Zone --format=json --project=$ProjectId | ConvertFrom-Json
    $vmIp = $vmInfo.networkInterfaces[0].accessConfigs[0].natIP
    Write-Host "   VM IP: $vmIp" -ForegroundColor Gray
    Write-Host "   Status: $($vmInfo.status)" -ForegroundColor Gray
} catch {
    Write-Host "   ❌ Could not find VM. Is it deployed?" -ForegroundColor Red
    exit 1
}

Write-Host ""

# 1. Test VM connectivity
Test-Component -Name "VM Connectivity" -Test {
    Test-NetConnection -ComputerName $vmIp -Port 8080 -WarningAction SilentlyContinue | Select-Object -ExpandProperty TcpTestSucceeded
} -SuccessMessage "VM is reachable on port 8080" -FailureMessage "Cannot reach VM on port 8080"

# 2. Test health endpoint
$healthData = $null
Test-Component -Name "Health API" -Test {
    try {
        $script:healthData = Invoke-RestMethod -Uri "http://${vmIp}:8080/health" -TimeoutSec 10
        $healthData.healthy -eq $true
    } catch { $false }
} -SuccessMessage "Health API responding (Peer: $($healthData.peer), Adapter: $($healthData.adapter))" `
  -FailureMessage "Health API not responding or unhealthy"

# 3. Test Fabric peer connectivity
Test-Component -Name "Fabric Peer Port (7051)" -Test {
    Test-NetConnection -ComputerName $vmIp -Port 7051 -WarningAction SilentlyContinue | Select-Object -ExpandProperty TcpTestSucceeded
} -SuccessMessage "Fabric peer port 7051 is open" -FailureMessage "Fabric peer port 7051 is not accessible"

# 4. Test local peer
Write-Host "`n🔍 Testing: Local Peer" -ForegroundColor Yellow
try {
    $localPeerRunning = docker ps --filter "name=peer0.org1.example.com" --format "{{.Status}}" 2>$null
    if ($localPeerRunning) {
        Write-Host "   ✅ Local peer is running" -ForegroundColor Green
        $testResults.Passed++
    } else {
        Write-Host "   ❌ Local peer is not running" -ForegroundColor Red
        Write-Host "   Run: docker-compose up -d" -ForegroundColor Yellow
        $testResults.Failed++
    }
} catch {
    Write-Host "   ⚠️  Could not check local peer (Docker not available?)" -ForegroundColor Yellow
    $testResults.Warnings++
}

# 5. Test channel membership
Write-Host "`n🔍 Testing: Channel Membership" -ForegroundColor Yellow
$localChannels = $null
$gcpChannels = $null

# Check local peer channels
if ($localPeerRunning) {
    try {
        $localChannelList = docker exec peer0.org1.example.com peer channel list 2>&1
        if ($localChannelList -match $ChannelName) {
            Write-Host "   ✅ Local peer on channel: $ChannelName" -ForegroundColor Green
            $testResults.Passed++
            $localChannels = $true
        } else {
            Write-Host "   ⚠️  Local peer not on channel: $ChannelName" -ForegroundColor Yellow
            $testResults.Warnings++
        }
    } catch {
        Write-Host "   ❌ Could not check local peer channels" -ForegroundColor Red
        $testResults.Failed++
    }
}

# Check GCP peer channels
try {
    $gcpChannelList = gcloud compute ssh $VmName --zone=$Zone --command="docker exec peer1.org1.example.com peer channel list 2>&1" --project=$ProjectId
    if ($gcpChannelList -match $ChannelName) {
        Write-Host "   ✅ GCP peer on channel: $ChannelName" -ForegroundColor Green
        $testResults.Passed++
        $gcpChannels = $true
    } else {
        Write-Host "   ⚠️  GCP peer not on channel: $ChannelName" -ForegroundColor Yellow
        Write-Host "   Run: ./setup-fabric-channel.sh $vmIp" -ForegroundColor Yellow
        $testResults.Warnings++
    }
} catch {
    Write-Host "   ❌ Could not check GCP peer channels" -ForegroundColor Red
    $testResults.Failed++
}

# 6. Test blockchain synchronization
if ($localChannels -and $gcpChannels) {
    Write-Host "`n🔍 Testing: Blockchain Synchronization" -ForegroundColor Yellow
    
    try {
        # Get block heights
        $localHeight = docker exec peer0.org1.example.com peer channel getinfo -c $ChannelName 2>&1 | Select-String "height: (\d+)" | ForEach-Object { $_.Matches[0].Groups[1].Value }
        $gcpHeightCmd = "docker exec peer1.org1.example.com peer channel getinfo -c $ChannelName 2>&1"
        $gcpHeightOutput = gcloud compute ssh $VmName --zone=$Zone --command=$gcpHeightCmd --project=$ProjectId
        $gcpHeight = $gcpHeightOutput | Select-String "height: (\d+)" | ForEach-Object { $_.Matches[0].Groups[1].Value }
        
        Write-Host "   Local blockchain height: $localHeight" -ForegroundColor Gray
        Write-Host "   GCP blockchain height: $gcpHeight" -ForegroundColor Gray
        
        if ($localHeight -eq $gcpHeight) {
            Write-Host "   ✅ Blockchains are synchronized!" -ForegroundColor Green
            $testResults.Passed++
        } else {
            Write-Host "   ⚠️  Blockchains are not synchronized (may sync soon)" -ForegroundColor Yellow
            $testResults.Warnings++
        }
    } catch {
        Write-Host "   ❌ Could not check blockchain heights" -ForegroundColor Red
        Write-Verbose "   Error: $_"
        $testResults.Failed++
    }
}

# 7. Test Pub/Sub connectivity
Write-Host "`n🔍 Testing: Pub/Sub Integration" -ForegroundColor Yellow

# Check if topic exists
Test-Component -Name "Pub/Sub Topic" -Test {
    $topics = gcloud pubsub topics list --project=$ProjectId --format="value(name)" 2>$null
    $topics -contains "projects/$ProjectId/topics/$Topic"
} -SuccessMessage "Topic '$Topic' exists" -FailureMessage "Topic '$Topic' not found"

# Check if subscription exists
Test-Component -Name "Pub/Sub Subscription" -Test {
    $subs = gcloud pubsub subscriptions list --project=$ProjectId --format="value(name)" 2>$null
    $subs -contains "projects/$ProjectId/subscriptions/fabric-ingestion"
} -SuccessMessage "Subscription 'fabric-ingestion' exists" -FailureMessage "Subscription not found"

# 8. Test end-to-end Pub/Sub flow
Write-Host "`n🔍 Testing: End-to-End Pub/Sub Flow" -ForegroundColor Yellow

$testId = "TEST_PS_$(Get-Random -Maximum 9999)"
$testMessage = @{
    id = $testId
    entity = "test"
    operation = "CREATE"
    data = @{
        source = "gcp-peer-test"
        timestamp = (Get-Date).ToString("yyyy-MM-ddTHH:mm:ss")
        test = $true
    }
} | ConvertTo-Json -Compress

Write-Host "   Publishing test message: $testId" -ForegroundColor Gray

# Publish message
try {
    gcloud pubsub topics publish $Topic --message="$testMessage" --project=$ProjectId
    Write-Host "   ✅ Message published to Pub/Sub" -ForegroundColor Green
    $testResults.Passed++
    
    # Wait for processing
    Write-Host "   ⏳ Waiting for blockchain processing (15 seconds)..." -ForegroundColor Gray
    Start-Sleep -Seconds 15
    
    # Check adapter logs
    Write-Host "   Checking adapter logs..." -ForegroundColor Gray
    $adapterLogs = gcloud compute ssh $VmName --zone=$Zone --command="sudo journalctl -u pubsub-fabric-adapter --since '1 minute ago' --no-pager | grep -i '$testId'" --project=$ProjectId 2>$null
    
    if ($adapterLogs) {
        Write-Host "   ✅ Message processed by adapter" -ForegroundColor Green
        $testResults.Passed++
        Write-Verbose "   Log entries:"
        Write-Verbose $adapterLogs
    } else {
        Write-Host "   ❌ Message not found in adapter logs" -ForegroundColor Red
        $testResults.Failed++
    }
} catch {
    Write-Host "   ❌ Failed to publish test message: $_" -ForegroundColor Red
    $testResults.Failed++
}

# 9. Test direct chaincode invocation
Write-Host "`n🔍 Testing: Direct Chaincode Invocation" -ForegroundColor Yellow

$directTestId = "TEST_DIRECT_$(Get-Random -Maximum 9999)"
$testData = @{
    id = $directTestId
    entity = "direct-test"
    operation = "CREATE"
    data = @{ source = "health-api-test" }
}

try {
    $response = Invoke-RestMethod -Method Post -Uri "http://${vmIp}:8080/test-message" `
        -Body ($testData | ConvertTo-Json) `
        -ContentType "application/json" `
        -TimeoutSec 30
    
    if ($response.success) {
        Write-Host "   ✅ Direct chaincode invocation successful" -ForegroundColor Green
        $testResults.Passed++
    } else {
        Write-Host "   ❌ Direct chaincode invocation failed" -ForegroundColor Red
        $testResults.Failed++
    }
} catch {
    Write-Host "   ❌ Could not invoke chaincode: $_" -ForegroundColor Red
    $testResults.Failed++
}

# 10. Test gossip communication
if ($localChannels -and $gcpChannels) {
    Write-Host "`n🔍 Testing: Gossip Protocol Communication" -ForegroundColor Yellow
    
    try {
        # Check for gossip messages in logs
        $gossipLogs = gcloud compute ssh $VmName --zone=$Zone --command="docker logs peer1.org1.example.com 2>&1 | grep -i gossip | tail -5" --project=$ProjectId
        
        if ($gossipLogs) {
            Write-Host "   ✅ Gossip protocol active" -ForegroundColor Green
            $testResults.Passed++
            Write-Verbose "   Recent gossip activity:"
            Write-Verbose $gossipLogs
        } else {
            Write-Host "   ⚠️  No recent gossip activity detected" -ForegroundColor Yellow
            $testResults.Warnings++
        }
    } catch {
        Write-Host "   ⚠️  Could not check gossip logs" -ForegroundColor Yellow
        $testResults.Warnings++
    }
}

# Summary
Write-Host "`n═══════════════════════════════════════════" -ForegroundColor Cyan
Write-Host "📊 Test Summary" -ForegroundColor Cyan
Write-Host "═══════════════════════════════════════════" -ForegroundColor Cyan
Write-Host "Total Tests: $($testResults.TotalTests)" -ForegroundColor White
Write-Host "✅ Passed: $($testResults.Passed)" -ForegroundColor Green
Write-Host "❌ Failed: $($testResults.Failed)" -ForegroundColor Red
Write-Host "⚠️  Warnings: $($testResults.Warnings)" -ForegroundColor Yellow

$successRate = if ($testResults.TotalTests -gt 0) { 
    [math]::Round(($testResults.Passed / $testResults.TotalTests) * 100, 2) 
} else { 0 }

Write-Host "`nSuccess Rate: $successRate%" -ForegroundColor $(if ($successRate -ge 80) { "Green" } elseif ($successRate -ge 60) { "Yellow" } else { "Red" })

# Recommendations
if ($testResults.Failed -gt 0 -or $testResults.Warnings -gt 0) {
    Write-Host "`n💡 Recommendations:" -ForegroundColor Yellow
    
    if (-not $localChannels -or -not $gcpChannels) {
        Write-Host "1. Create and join channel:" -ForegroundColor White
        Write-Host "   ./setup-fabric-channel.sh $vmIp" -ForegroundColor Gray
    }
    
    if ($testResults.Failed -contains "Health API") {
        Write-Host "2. Check VM startup logs:" -ForegroundColor White
        Write-Host "   gcloud compute ssh $VmName --zone=$Zone --command='sudo journalctl -u google-startup-scripts.service' --project=$ProjectId" -ForegroundColor Gray
    }
    
    if ($localHeight -ne $gcpHeight) {
        Write-Host "3. Check peer connectivity:" -ForegroundColor White
        Write-Host "   Ensure port 7051 is open between peers" -ForegroundColor Gray
        Write-Host "   Check if local peer IP is correct: $((Invoke-WebRequest -Uri 'https://ipinfo.io/ip').Content.Trim())" -ForegroundColor Gray
    }
}

# Continuous monitoring mode
if ($ContinuousMonitoring) {
    Write-Host "`n📊 Starting continuous monitoring mode (Ctrl+C to stop)..." -ForegroundColor Yellow
    Write-Host "═══════════════════════════════════════════════════════════" -ForegroundColor Gray
    
    while ($true) {
        Start-Sleep -Seconds 30
        
        # Get current heights
        try {
            $localH = docker exec peer0.org1.example.com peer channel getinfo -c $ChannelName 2>&1 | Select-String "height: (\d+)" | ForEach-Object { $_.Matches[0].Groups[1].Value }
            $gcpCmd = "docker exec peer1.org1.example.com peer channel getinfo -c $ChannelName 2>&1"
            $gcpH = gcloud compute ssh $VmName --zone=$Zone --command=$gcpCmd --project=$ProjectId | Select-String "height: (\d+)" | ForEach-Object { $_.Matches[0].Groups[1].Value }
            
            $timestamp = Get-Date -Format "HH:mm:ss"
            $syncStatus = if ($localH -eq $gcpH) { "✅ SYNCED" } else { "⚠️  DIFF" }
            
            Write-Host "[$timestamp] Local: $localH | GCP: $gcpH | Status: $syncStatus" -ForegroundColor Gray
        } catch {
            Write-Host "[$timestamp] ❌ Error reading block heights" -ForegroundColor Red
        }
    }
}

Write-Host "`n✅ Testing complete!" -ForegroundColor Green