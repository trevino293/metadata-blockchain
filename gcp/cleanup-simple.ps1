# cleanup-simple.ps1 - Clean up the simple GCP setup (Windows PowerShell)

param(
    [Parameter(Mandatory=$true)]
    [string]$ProjectId,
    [string]$Zone = "us-central1-a",
    [switch]$Force
)

Write-Host "🧹 Cleaning up GCP resources..." -ForegroundColor Yellow
Write-Host "Project: $ProjectId" -ForegroundColor Cyan
Write-Host "Zone: $Zone" -ForegroundColor Cyan
Write-Host ""

# Warning unless -Force is used
if (-not $Force) {
    Write-Host "⚠️  This will delete all resources created by simple-gcp-deploy.ps1" -ForegroundColor Red
    Write-Host ""
    Write-Host "Resources to be deleted:" -ForegroundColor Yellow
    Write-Host "• VM instance: blockchain-node" -ForegroundColor Gray
    Write-Host "• Firewall rule: allow-blockchain" -ForegroundColor Gray
    Write-Host "• Pub/Sub subscription: blockchain-sub" -ForegroundColor Gray
    Write-Host "• Pub/Sub topic: metadata-events" -ForegroundColor Gray
    Write-Host ""
    
    $confirmation = Read-Host "Type 'yes' to continue"
    if ($confirmation -ne 'yes') {
        Write-Host "Cleanup cancelled." -ForegroundColor Yellow
        exit 0
    }
}

Write-Host ""

# Get current resources before deletion
Write-Host "📋 Current resources:" -ForegroundColor Yellow
try {
    Write-Host "Checking VM status..." -ForegroundColor Gray
    $vmStatus = gcloud compute instances describe blockchain-node --zone=$Zone --format="value(status)" --project=$ProjectId 2>$null
    if ($vmStatus) {
        Write-Host "• VM blockchain-node: $vmStatus" -ForegroundColor Cyan
    } else {
        Write-Host "• VM blockchain-node: not found" -ForegroundColor Gray
    }
} catch {
    Write-Host "• VM blockchain-node: not found" -ForegroundColor Gray
}

try {
    Write-Host "Checking Pub/Sub resources..." -ForegroundColor Gray
    $topics = gcloud pubsub topics list --filter="name:metadata-events" --format="value(name)" --project=$ProjectId 2>$null
    if ($topics) {
        Write-Host "• Pub/Sub topic: metadata-events exists" -ForegroundColor Cyan
    }
    
    $subscriptions = gcloud pubsub subscriptions list --filter="name:blockchain-sub" --format="value(name)" --project=$ProjectId 2>$null
    if ($subscriptions) {
        Write-Host "• Pub/Sub subscription: blockchain-sub exists" -ForegroundColor Cyan
    }
} catch {
    Write-Host "• Pub/Sub resources: checking..." -ForegroundColor Gray
}

Write-Host ""

# 1. Delete VM
Write-Host "1. Deleting VM..." -ForegroundColor Yellow
try {
    gcloud compute instances delete blockchain-node `
        --zone=$Zone `
        --project=$ProjectId `
        --quiet 2>$null
    
    if ($LASTEXITCODE -eq 0) {
        Write-Host "✅ VM blockchain-node deleted" -ForegroundColor Green
    } else {
        Write-Host "⚠️ VM blockchain-node may not exist or already deleted" -ForegroundColor Yellow
    }
} catch {
    Write-Host "⚠️ Error deleting VM: $($_.Exception.Message)" -ForegroundColor Yellow
}

# 2. Delete firewall rule
Write-Host "2. Deleting firewall rule..." -ForegroundColor Yellow
try {
    gcloud compute firewall-rules delete allow-blockchain `
        --project=$ProjectId `
        --quiet 2>$null
    
    if ($LASTEXITCODE -eq 0) {
        Write-Host "✅ Firewall rule allow-blockchain deleted" -ForegroundColor Green
    } else {
        Write-Host "⚠️ Firewall rule may not exist or already deleted" -ForegroundColor Yellow
    }
} catch {
    Write-Host "⚠️ Error deleting firewall rule: $($_.Exception.Message)" -ForegroundColor Yellow
}

# 3. Delete Pub/Sub subscription
Write-Host "3. Deleting Pub/Sub subscription..." -ForegroundColor Yellow
try {
    gcloud pubsub subscriptions delete blockchain-sub `
        --project=$ProjectId `
        --quiet 2>$null
    
    if ($LASTEXITCODE -eq 0) {
        Write-Host "✅ Pub/Sub subscription blockchain-sub deleted" -ForegroundColor Green
    } else {
        Write-Host "⚠️ Subscription may not exist or already deleted" -ForegroundColor Yellow
    }
} catch {
    Write-Host "⚠️ Error deleting subscription: $($_.Exception.Message)" -ForegroundColor Yellow
}

# 4. Delete Pub/Sub topic
Write-Host "4. Deleting Pub/Sub topic..." -ForegroundColor Yellow
try {
    gcloud pubsub topics delete metadata-events `
        --project=$ProjectId `
        --quiet 2>$null
    
    if ($LASTEXITCODE -eq 0) {
        Write-Host "✅ Pub/Sub topic metadata-events deleted" -ForegroundColor Green
    } else {
        Write-Host "⚠️ Topic may not exist or already deleted" -ForegroundColor Yellow
    }
} catch {
    Write-Host "⚠️ Error deleting topic: $($_.Exception.Message)" -ForegroundColor Yellow
}

# 5. Clean up local files
Write-Host "5. Cleaning up local files..." -ForegroundColor Yellow
$filesToDelete = @(
    "connection-info.txt",
    "blockchain-monitor.html",
    "startup-script.txt"
)

foreach ($file in $filesToDelete) {
    if (Test-Path $file) {
        Remove-Item $file -Force
        Write-Host "✅ Deleted local file: $file" -ForegroundColor Green
    }
}

Write-Host ""
Write-Host "✅ Cleanup complete!" -ForegroundColor Green
Write-Host ""

# Verify cleanup
Write-Host "🔍 Verification:" -ForegroundColor Yellow
Write-Host "Checking remaining resources..." -ForegroundColor Gray

# Check for remaining VMs
$remainingVMs = gcloud compute instances list --filter="name:blockchain*" --format="value(name)" --project=$ProjectId 2>$null
if ($remainingVMs) {
    Write-Host "⚠️ Remaining VMs found: $remainingVMs" -ForegroundColor Yellow
} else {
    Write-Host "✅ No blockchain VMs remaining" -ForegroundColor Green
}

# Check for remaining firewall rules
$remainingRules = gcloud compute firewall-rules list --filter="name:*blockchain*" --format="value(name)" --project=$ProjectId 2>$null
if ($remainingRules) {
    Write-Host "⚠️ Remaining firewall rules: $remainingRules" -ForegroundColor Yellow
} else {
    Write-Host "✅ No blockchain firewall rules remaining" -ForegroundColor Green
}

# Check for remaining Pub/Sub resources
$remainingTopics = gcloud pubsub topics list --filter="name:metadata*" --format="value(name)" --project=$ProjectId 2>$null
if ($remainingTopics) {
    Write-Host "⚠️ Remaining Pub/Sub topics: $remainingTopics" -ForegroundColor Yellow
} else {
    Write-Host "✅ No metadata Pub/Sub topics remaining" -ForegroundColor Green
}

Write-Host ""
Write-Host "📊 Cost Impact:" -ForegroundColor Yellow
Write-Host "• VM costs stopped (was ~$0.006/hour for e2-small)" -ForegroundColor Gray
Write-Host "• Pub/Sub costs stopped (was ~$0.40 per million messages)" -ForegroundColor Gray
Write-Host "• No ongoing charges for deleted resources" -ForegroundColor Gray

Write-Host ""
Write-Host "💡 Next steps:" -ForegroundColor Yellow
Write-Host "• Check GCP Console to verify all resources are deleted" -ForegroundColor Gray
Write-Host "• Review billing to confirm no unexpected charges" -ForegroundColor Gray
Write-Host "• Re-run deployment script if you want to recreate the setup" -ForegroundColor Gray

Write-Host ""
Write-Host "🔗 Useful links:" -ForegroundColor Yellow
Write-Host "• GCP Console: https://console.cloud.google.com/compute/instances?project=$ProjectId" -ForegroundColor Gray
Write-Host "• Billing: https://console.cloud.google.com/billing?project=$ProjectId" -ForegroundColor Gray