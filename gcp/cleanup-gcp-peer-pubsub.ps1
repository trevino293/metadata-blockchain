# cleanup-gcp-peer-with-pubsub.ps1
# Clean up GCP Fabric peer deployment with Pub/Sub resources

param(
    [Parameter(Mandatory=$true)]
    [string]$ProjectId,
    
    [string]$VmName = "fabric-peer",
    [string]$Topic = "blockchain-metadata",
    [string]$Subscription = "fabric-ingestion",
    [string[]]$Zones = @("us-central1-a", "us-central1-b", "us-central1-c", "us-east1-b", "us-west1-a"),
    [switch]$KeepFirewallRules,
    [switch]$KeepPubSub,
    [switch]$Force
)

Write-Host "🧹 Cleanup Script for GCP Fabric Peer with Pub/Sub" -ForegroundColor Yellow
Write-Host "===================================================" -ForegroundColor Yellow

# Show what will be deleted
Write-Host "`nResources to be deleted:" -ForegroundColor Yellow
Write-Host "  • VM Instance: $VmName" -ForegroundColor Gray
if (-not $KeepPubSub) {
    Write-Host "  • Pub/Sub Topic: $Topic" -ForegroundColor Gray
    Write-Host "  • Pub/Sub Subscription: $Subscription" -ForegroundColor Gray
}
if (-not $KeepFirewallRules) {
    Write-Host "  • Firewall Rule: allow-fabric" -ForegroundColor Gray
}
Write-Host "  • Local config directory: fabric-pubsub-config\" -ForegroundColor Gray

# Confirm deletion
if (-not $Force) {
    Write-Host "`n⚠️  This will permanently delete the above resources!" -ForegroundColor Red
    $confirm = Read-Host "Are you sure you want to continue? (yes/no)"
    if ($confirm -ne "yes") {
        Write-Host "`nCleanup cancelled" -ForegroundColor Yellow
        exit
    }
}

$errorCount = 0

# 1. Delete VM instance
Write-Host "`n1. Deleting VM instance..." -ForegroundColor Yellow

$vmDeleted = $false
foreach ($zone in $Zones) {
    try {
        $vmExists = gcloud compute instances describe $VmName --zone=$zone --project=$ProjectId 2>$null
        if ($LASTEXITCODE -eq 0) {
            Write-Host "   Found VM in zone: $zone" -ForegroundColor Gray
            Write-Host "   Deleting $VmName..." -ForegroundColor Gray
            
            gcloud compute instances delete $VmName --zone=$zone --quiet --project=$ProjectId
            
            if ($LASTEXITCODE -eq 0) {
                Write-Host "   ✅ VM deleted successfully" -ForegroundColor Green
                $vmDeleted = $true
                break
            } else {
                Write-Host "   ❌ Failed to delete VM" -ForegroundColor Red
                $errorCount++
            }
        }
    } catch {
        # VM not in this zone, continue
    }
}

if (-not $vmDeleted) {
    Write-Host "   ℹ️ VM not found in any zone (may already be deleted)" -ForegroundColor Gray
}

# 2. Delete Pub/Sub resources
if (-not $KeepPubSub) {
    Write-Host "`n2. Deleting Pub/Sub resources..." -ForegroundColor Yellow
    
    # Delete subscription first (must be done before topic)
    Write-Host "   Deleting subscription: $Subscription" -ForegroundColor Gray
    gcloud pubsub subscriptions delete $Subscription --quiet --project=$ProjectId 2>$null
    if ($LASTEXITCODE -eq 0) {
        Write-Host "   ✅ Subscription deleted" -ForegroundColor Green
    } elseif ($LASTEXITCODE -eq 1) {
        Write-Host "   ℹ️ Subscription not found (may already be deleted)" -ForegroundColor Gray
    } else {
        Write-Host "   ❌ Failed to delete subscription" -ForegroundColor Red
        $errorCount++
    }
    
    # Delete topic
    Write-Host "   Deleting topic: $Topic" -ForegroundColor Gray
    gcloud pubsub topics delete $Topic --quiet --project=$ProjectId 2>$null
    if ($LASTEXITCODE -eq 0) {
        Write-Host "   ✅ Topic deleted" -ForegroundColor Green
    } elseif ($LASTEXITCODE -eq 1) {
        Write-Host "   ℹ️ Topic not found (may already be deleted)" -ForegroundColor Gray
    } else {
        Write-Host "   ❌ Failed to delete topic" -ForegroundColor Red
        $errorCount++
    }
} else {
    Write-Host "`n2. Keeping Pub/Sub resources (as requested)" -ForegroundColor Gray
}

# 3. Delete firewall rules
if (-not $KeepFirewallRules) {
    Write-Host "`n3. Deleting firewall rules..." -ForegroundColor Yellow
    
    Write-Host "   Deleting firewall rule: allow-fabric" -ForegroundColor Gray
    gcloud compute firewall-rules delete allow-fabric --quiet --project=$ProjectId 2>$null
    if ($LASTEXITCODE -eq 0) {
        Write-Host "   ✅ Firewall rule deleted" -ForegroundColor Green
    } elseif ($LASTEXITCODE -eq 1) {
        Write-Host "   ℹ️ Firewall rule not found (may already be deleted)" -ForegroundColor Gray
    } else {
        Write-Host "   ❌ Failed to delete firewall rule" -ForegroundColor Red
        $errorCount++
    }
} else {
    Write-Host "`n3. Keeping firewall rules (as requested)" -ForegroundColor Gray
}

# 4. Clean up local files
Write-Host "`n4. Cleaning up local files..." -ForegroundColor Yellow

$localDir = "fabric-pubsub-config"
if (Test-Path $localDir) {
    try {
        Remove-Item -Path $localDir -Recurse -Force -ErrorAction Stop
        Write-Host "   ✅ Deleted directory: $localDir" -ForegroundColor Green
    } catch {
        Write-Host "   ❌ Failed to delete directory: $localDir" -ForegroundColor Red
        Write-Host "   Error: $_" -ForegroundColor Red
        $errorCount++
    }
} else {
    Write-Host "   ℹ️ Directory not found: $localDir" -ForegroundColor Gray
}

# 5. Check for any remaining resources
Write-Host "`n5. Checking for remaining resources..." -ForegroundColor Yellow

# Check for any VMs with 'fabric' in the name
Write-Host "   Checking for other Fabric-related VMs..." -ForegroundColor Gray
$remainingVMs = gcloud compute instances list --filter="name~'fabric'" --project=$ProjectId --format="value(name,zone)" 2>$null
if ($remainingVMs) {
    Write-Host "   ⚠️ Found other Fabric-related VMs:" -ForegroundColor Yellow
    $remainingVMs | ForEach-Object { Write-Host "      $_" -ForegroundColor Gray }
} else {
    Write-Host "   ✅ No other Fabric VMs found" -ForegroundColor Green
}

# Check for any Pub/Sub topics with 'blockchain' or 'metadata' in the name
if (-not $KeepPubSub) {
    Write-Host "   Checking for other blockchain-related Pub/Sub resources..." -ForegroundColor Gray
    $remainingTopics = gcloud pubsub topics list --filter="name~'blockchain|metadata'" --project=$ProjectId --format="value(name)" 2>$null
    if ($remainingTopics) {
        Write-Host "   ℹ️ Found other blockchain-related topics:" -ForegroundColor Yellow
        $remainingTopics | ForEach-Object { Write-Host "      $_" -ForegroundColor Gray }
    }
}

# 6. Summary
Write-Host "`n══════════════════════════════════════════════════" -ForegroundColor Green
if ($errorCount -eq 0) {
    Write-Host "✅ Cleanup completed successfully!" -ForegroundColor Green
} else {
    Write-Host "⚠️ Cleanup completed with $errorCount errors" -ForegroundColor Yellow
}
Write-Host "══════════════════════════════════════════════════" -ForegroundColor Green

# Show next steps
Write-Host "`nNext steps:" -ForegroundColor Yellow
Write-Host "1. To deploy again:" -ForegroundColor White
Write-Host '   $ip = (Invoke-WebRequest -Uri "https://ipinfo.io/ip").Content.Trim()' -ForegroundColor Gray
Write-Host "   .\gcp-fabric-peer-with-pubsub.ps1 -ProjectId `"$ProjectId`" -LocalPeerIP `$ip" -ForegroundColor Gray

if ($KeepPubSub) {
    Write-Host "`n2. Pub/Sub resources were kept. To delete them later:" -ForegroundColor White
    Write-Host "   gcloud pubsub subscriptions delete $Subscription --project=$ProjectId" -ForegroundColor Gray
    Write-Host "   gcloud pubsub topics delete $Topic --project=$ProjectId" -ForegroundColor Gray
}

if ($KeepFirewallRules) {
    Write-Host "`n3. Firewall rules were kept. To delete them later:" -ForegroundColor White
    Write-Host "   gcloud compute firewall-rules delete allow-fabric --project=$ProjectId" -ForegroundColor Gray
}

# Optional: Show commands to verify cleanup
Write-Host "`n💡 To verify cleanup:" -ForegroundColor Yellow
Write-Host "   # Check VMs" -ForegroundColor Gray
Write-Host "   gcloud compute instances list --project=$ProjectId" -ForegroundColor Gray
Write-Host "   # Check Pub/Sub" -ForegroundColor Gray
Write-Host "   gcloud pubsub topics list --project=$ProjectId" -ForegroundColor Gray
Write-Host "   gcloud pubsub subscriptions list --project=$ProjectId" -ForegroundColor Gray
Write-Host "   # Check firewall rules" -ForegroundColor Gray
Write-Host "   gcloud compute firewall-rules list --project=$ProjectId" -ForegroundColor Gray