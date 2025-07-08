Write-Host "=== Fixing COBOL-Blockchain Adapter Startup ===" -ForegroundColor Green

# First, let's check what's actually running
Write-Host "`nChecking current processes..." -ForegroundColor Yellow
docker exec cobol-metadata-node ps aux

# Kill the tail process to stop the container from just idling
Write-Host "`nStopping idle process..." -ForegroundColor Yellow
docker exec cobol-metadata-node pkill -f "tail -f /dev/null"

# Now check if the Python adapter file exists
Write-Host "`nChecking for adapter file..." -ForegroundColor Yellow
docker exec cobol-metadata-node ls -la /app/adapters/

# The log file has lots of data, let's make sure the adapter can read it
Write-Host "`nChecking log file..." -ForegroundColor Yellow
docker exec cobol-metadata-node bash -c "wc -l /app/logs/blockchain-writes.log"

# Now let's create a startup script that properly runs the adapter
Write-Host "`nCreating proper startup script..." -ForegroundColor Yellow
@'
#!/bin/bash
echo "Starting COBOL-Blockchain Integration Services..."

# Create necessary directories
mkdir -p /app/data /app/logs /app/pipes

# Start the Python adapter
echo "Starting Python adapter on port 8080..."
cd /app
python3 /app/adapters/cobol-fabric-adapter.py
'@ | Out-File -FilePath "start-adapter.sh" -Encoding UTF8

# Copy and make executable
docker cp start-adapter.sh cobol-metadata-node:/app/start-adapter.sh
docker exec cobol-metadata-node chmod +x /app/start-adapter.sh

# Now let's update the start-services.sh to run the adapter
Write-Host "`nUpdating start-services.sh..." -ForegroundColor Yellow
@'
#!/bin/bash

# Initialize environment
echo "Starting COBOL-Blockchain Metadata Services..."

# Create necessary directories
mkdir -p data logs pipes /tmp

# Create named pipes
./scripts/create-pipes.sh 2>/dev/null || echo "Pipes already exist"

echo "Starting services from $(pwd)..."
echo "Data directory: $(pwd)/data"
echo "Logs directory: $(pwd)/logs"

# Start the Python adapter instead of just tailing
echo "Starting blockchain adapter..."
python3 /app/adapters/cobol-fabric-adapter.py
'@ | Out-File -FilePath "new-start-services.sh" -Encoding UTF8

docker cp new-start-services.sh cobol-metadata-node:/app/start-services.sh
docker exec cobol-metadata-node chmod +x /app/start-services.sh

# Restart the container to pick up the new startup script
Write-Host "`nRestarting container..." -ForegroundColor Yellow
docker restart cobol-metadata-node

# Wait for it to start
Write-Host "Waiting for container to restart..." -ForegroundColor Yellow
Start-Sleep -Seconds 5

# Check if the adapter is now running
Write-Host "`nChecking if adapter is running..." -ForegroundColor Yellow
docker exec cobol-metadata-node ps aux | Select-String python

# Wait a bit more for the adapter to process the log file
Start-Sleep -Seconds 5

# Test the API
Write-Host "`nTesting API..." -ForegroundColor Yellow
try {
    $response = Invoke-RestMethod -Uri "http://localhost:8080/api/transactions" -Method GET -ErrorAction Stop
    Write-Host "Total transactions: $($response.total)" -ForegroundColor Green
    
    if ($response.total -gt 2) {
        Write-Host "Success! The adapter is now reading the log file!" -ForegroundColor Green
        Write-Host "Sample transactions:" -ForegroundColor Cyan
        $response.transactions | Select-Object -First 5 | ForEach-Object {
            Write-Host "  - $($_.recordId) | $($_.file) | $($_.operation)" -ForegroundColor Gray
        }
    } else {
        Write-Host "Still only showing initial transactions." -ForegroundColor Yellow
        Write-Host "Checking adapter output..." -ForegroundColor Yellow
        docker logs cobol-metadata-node --tail 50
    }
} catch {
    Write-Host "API not responding. Checking container logs..." -ForegroundColor Red
    docker logs cobol-metadata-node --tail 30
}

# Clean up
Remove-Item "start-adapter.sh" -ErrorAction SilentlyContinue
Remove-Item "new-start-services.sh" -ErrorAction SilentlyContinue

Write-Host "`n=== Instructions ===" -ForegroundColor Green
Write-Host "1. Refresh your dashboard in the browser" -ForegroundColor Cyan
Write-Host "2. You should now see all the transactions from the log file" -ForegroundColor Cyan
Write-Host "3. To monitor adapter activity: docker logs -f cobol-metadata-node" -ForegroundColor Cyan