# Save as: cleanup-and-reset.ps1

Write-Host "=== Cleaning Up Existing System ===" -ForegroundColor Yellow

# Stop all Python processes
docker exec cobol-metadata-node pkill -9 -f "python3" 2>$null

# Backup and clear logs
docker exec cobol-metadata-node bash -c @'
# Backup if needed
if [ -f /app/logs/fabric-transactions.log ]; then
    cp /app/logs/fabric-transactions.log /app/logs/backup-$(date +%Y%m%d-%H%M%S).log
fi

# Clear everything
> /app/logs/fabric-transactions.log
> /app/logs/blockchain-writes.log
> /app/logs/adapter.log
rm -f /app/data/metadata.log
rm -f /app/data/sequence.dat
rm -rf /app/pipes/*
mkdir -p /app/pipes /app/data /app/logs
'@

Write-Host "✅ System cleaned" -ForegroundColor Green