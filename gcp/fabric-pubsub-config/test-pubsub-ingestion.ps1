# test-pubsub-ingestion.ps1
param(
    [string]$ProjectId = "metadata-blockchain",
    [string]$Topic = "blockchain-metadata",
    [string]$VmIp = "34.170.20.180"
)

Write-Host "Testing Pub/Sub to Fabric Ingestion" -ForegroundColor Green

# 1. Check health
Write-Host "
1. Checking system health..." -ForegroundColor Yellow
$health = Invoke-RestMethod -Uri "http://$VmIp:8080/health"
Write-Host "   Peer: $($health.peer)" -ForegroundColor Gray
Write-Host "   Adapter: $($health.adapter)" -ForegroundColor Gray

# 2. Send test message via Pub/Sub
Write-Host "
2. Publishing test message to Pub/Sub..." -ForegroundColor Yellow
$testMessage = @{
    id = "TEST_$(Get-Random -Maximum 9999)"
    entity = "customer"
    operation = "CREATE"
    data = @{
        name = "Test Customer"
        source = "PowerShell Test"
        timestamp = (Get-Date).ToString("yyyy-MM-ddTHH:mm:ss")
    }
} | ConvertTo-Json -Compress

gcloud pubsub topics publish $Topic --message="$testMessage" --project=$ProjectId

Write-Host "   ✅ Message published" -ForegroundColor Green

# 3. Wait for processing
Write-Host "
3. Waiting for blockchain processing (10 seconds)..." -ForegroundColor Yellow
Start-Sleep -Seconds 10

# 4. Check adapter logs
Write-Host "
4. Recent adapter logs:" -ForegroundColor Yellow
$logs = gcloud compute ssh $VmName --zone=us-central1-a --command="sudo journalctl -u pubsub-fabric-adapter -n 10 --no-pager" --project=$ProjectId
Write-Host $logs

Write-Host "
✅ Test complete!" -ForegroundColor Green
