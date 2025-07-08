# Save as: deploy-scripts.ps1

Write-Host "=== Deploying Management Scripts ===" -ForegroundColor Green

# Start script
@'
#!/bin/bash
echo "Starting COBOL-Blockchain Event System..."
mkdir -p /app/pipes /app/logs
python3 /app/adapters/async-crud-adapter.py > /app/logs/adapter.log 2>&1 &
echo $! > /app/adapter.pid
echo "Adapter started with PID: $(cat /app/adapter.pid)"
tail -f /app/logs/adapter.log
'@ | Out-File -FilePath "start-system.sh" -Encoding UTF8

# Stop script  
@'
#!/bin/bash
if [ -f /app/adapter.pid ]; then
    kill $(cat /app/adapter.pid)
    rm /app/adapter.pid
    echo "Adapter stopped"
else
    echo "No adapter running"
fi
'@ | Out-File -FilePath "stop-system.sh" -Encoding UTF8

# Status script
@'
#!/bin/bash
echo "=== System Status ==="
if [ -f /app/adapter.pid ] && ps -p $(cat /app/adapter.pid) > /dev/null; then
    echo "Adapter: RUNNING (PID: $(cat /app/adapter.pid))"
else
    echo "Adapter: STOPPED"
fi
echo ""
echo "Transaction count: $(wc -l < /app/logs/fabric-transactions.log 2>/dev/null || echo 0)"
echo "Latest transactions:"
tail -3 /app/logs/fabric-transactions.log 2>/dev/null | jq -r '.tx_id + " | " + .args[1] + " | " + .args[3]' 2>/dev/null
'@ | Out-File -FilePath "status.sh" -Encoding UTF8

# Deploy all scripts
docker cp start-system.sh cobol-metadata-node:/app/scripts/
docker cp stop-system.sh cobol-metadata-node:/app/scripts/
docker cp status.sh cobol-metadata-node:/app/scripts/
docker exec cobol-metadata-node chmod +x /app/scripts/*.sh

Remove-Item "start-system.sh", "stop-system.sh", "status.sh"
Write-Host "✅ Scripts deployed" -ForegroundColor Green