# find-and-destroy-all-data.ps1 - Find and delete ALL .dat files including ones with spaces

Write-Host "=== FIND AND DESTROY ALL DATA FILES ===" -ForegroundColor Red
Write-Host "This will find and delete ALL .dat files, including ones with spaces in names" -ForegroundColor Yellow
Write-Host ""

# Step 1: Find all .dat files (including ones with spaces)
Write-Host "1. Finding all .dat files in the container..." -ForegroundColor Yellow
$allDatFiles = docker exec cobol-metadata-node bash -c 'find /app/data -name "*.dat" -type f'

Write-Host "   📁 Found .dat files:" -ForegroundColor Cyan
if ($allDatFiles) {
    $allDatFiles.Split("`n") | ForEach-Object {
        if ($_.Trim()) {
            Write-Host "      • '$($_.Trim())'" -ForegroundColor Gray
        }
    }
} else {
    Write-Host "      (no .dat files found)" -ForegroundColor Gray
}

# Step 2: Show file sizes
Write-Host ""
Write-Host "2. Checking file sizes..." -ForegroundColor Yellow
$fileSizes = docker exec cobol-metadata-node bash -c 'find /app/data -name "*.dat" -type f -exec wc -l {} \;'

Write-Host "   📊 Current file sizes:" -ForegroundColor Cyan
if ($fileSizes) {
    $fileSizes.Split("`n") | ForEach-Object {
        if ($_.Trim()) {
            $line = $_.Trim()
            if ($line -match "^\s*0\s") {
                Write-Host "      ✅ $line (empty)" -ForegroundColor Green
            } else {
                Write-Host "      ❌ $line (has data)" -ForegroundColor Red
            }
        }
    }
} else {
    Write-Host "      (no files to check)" -ForegroundColor Gray
}

Write-Host ""
$confirmation = Read-Host "Type 'DESTROY' to delete ALL .dat files found above"
if ($confirmation -ne 'DESTROY') {
    Write-Host "Operation cancelled." -ForegroundColor Yellow
    exit 0
}

Write-Host ""
Write-Host "🔥 DESTROYING ALL .DAT FILES..." -ForegroundColor Red

# Step 3: Kill all processes first
Write-Host "3. Killing all processes..." -ForegroundColor Yellow
docker exec cobol-metadata-node bash -c 'pkill -9 python3 2>/dev/null; pkill -9 python 2>/dev/null; pkill -9 adapter 2>/dev/null; pkill -9 cobol 2>/dev/null'

# Step 4: Delete ALL .dat files using find (handles spaces and special chars)
Write-Host "4. Deleting ALL .dat files..." -ForegroundColor Red
docker exec cobol-metadata-node bash -c 'find /app/data -name "*.dat" -type f -delete'

# Step 5: Also clean any log files
Write-Host "5. Cleaning log files..." -ForegroundColor Yellow
docker exec cobol-metadata-node bash -c 'find /app/logs -name "*.log" -type f -delete'

# Step 6: Remove any other data-related files
Write-Host "6. Removing other data files..." -ForegroundColor Yellow
docker exec cobol-metadata-node bash -c '
    rm -f /app/data/*.idx
    rm -f /app/data/*.log  
    rm -f /app/data/metadata.*
    rm -f /app/data/sequence.*
    rm -f /app/*.pid
    rm -rf /app/pipes/*
'

# Step 7: Ensure directories exist (but don't recreate files)
Write-Host "7. Ensuring directories exist..." -ForegroundColor Green
docker exec cobol-metadata-node bash -c '
    # Just ensure directories exist
    mkdir -p /app/logs /app/data /app/pipes
    
    echo "Directories ready (no files recreated)"
'

# Step 8: Final verification
Write-Host ""
Write-Host "8. Final verification..." -ForegroundColor Yellow

# Check for any remaining .dat files
$remainingDat = docker exec cobol-metadata-node bash -c 'find /app/data -name "*.dat" -type f'
Write-Host "   📁 Remaining .dat files:" -ForegroundColor Cyan
if ($remainingDat) {
    $remainingDat.Split("`n") | ForEach-Object {
        if ($_.Trim()) {
            Write-Host "      ❌ '$($_.Trim())' (still exists!)" -ForegroundColor Red
        }
    }
} else {
    Write-Host "      ✅ No .dat files found (perfect!)" -ForegroundColor Green
}

# Check for any remaining .log files
$remainingLog = docker exec cobol-metadata-node bash -c 'find /app/logs -name "*.log" -type f'
Write-Host "   📁 Remaining .log files:" -ForegroundColor Cyan
if ($remainingLog) {
    $remainingLog.Split("`n") | ForEach-Object {
        if ($_.Trim()) {
            Write-Host "      ❌ '$($_.Trim())' (still exists!)" -ForegroundColor Red
        }
    }
} else {
    Write-Host "      ✅ No .log files found (perfect!)" -ForegroundColor Green
}

# Check directory contents
$dataContents = docker exec cobol-metadata-node bash -c 'ls -la /app/data/ 2>/dev/null | wc -l'
$logContents = docker exec cobol-metadata-node bash -c 'ls -la /app/logs/ 2>/dev/null | wc -l'

Write-Host "   📊 Directory status:" -ForegroundColor Cyan
if ($dataContents -le 3) {  # . and .. plus maybe one more
    Write-Host "      ✅ /app/data is empty (or nearly empty)" -ForegroundColor Green
} else {
    Write-Host "      ⚠️ /app/data still has files" -ForegroundColor Yellow
}

if ($logContents -le 3) {  # . and .. plus maybe one more
    Write-Host "      ✅ /app/logs is empty (or nearly empty)" -ForegroundColor Green
} else {
    Write-Host "      ⚠️ /app/logs still has files" -ForegroundColor Yellow
}

Write-Host ""
Write-Host "🎯 DESTROY operation complete!" -ForegroundColor Green
Write-Host ""
Write-Host "📋 What happened:" -ForegroundColor Yellow
Write-Host "• Used 'find' to locate ALL .dat files (including ones with spaces)" -ForegroundColor Gray
Write-Host "• Deleted ALL found .dat files completely" -ForegroundColor Gray  
Write-Host "• Deleted ALL log files completely" -ForegroundColor Gray
Write-Host "• Left directories intact (no files recreated)" -ForegroundColor Gray
Write-Host ""
Write-Host "✨ Data and logs are now completely wiped!" -ForegroundColor Green
Write-Host ""
Write-Host "🚀 Next steps:" -ForegroundColor Yellow
Write-Host "1. Files will be created automatically when you run the system" -ForegroundColor Cyan
Write-Host "2. Run: .\run-system.ps1 (to start fresh)" -ForegroundColor Cyan
Write-Host "3. Test with: .\tests\test-cobol-blockchain.ps1 -TestRecords 2" -ForegroundColor Cyan