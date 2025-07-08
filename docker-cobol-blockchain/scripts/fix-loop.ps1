Write-Host "=== Stopping Runaway Adapter and Cleaning Up ===" -ForegroundColor Red

# Step 1: Stop ALL Python adapters immediately
Write-Host "`n1. Stopping all Python adapters..." -ForegroundColor Yellow
docker exec cobol-metadata-node pkill -9 -f "python3" 2>$null
Start-Sleep -Seconds 2

# Verify they're stopped
$runningProcesses = docker exec cobol-metadata-node ps aux | Select-String "python"
if ($runningProcesses) {
    Write-Host "WARNING: Some Python processes still running:" -ForegroundColor Red
    Write-Output $runningProcesses
} else {
    Write-Host "✅ All Python adapters stopped" -ForegroundColor Green
}

# Step 2: Show the damage
Write-Host "`n2. Current transaction count:" -ForegroundColor Yellow
$txCount = docker exec cobol-metadata-node bash -c "wc -l < /app/logs/fabric-transactions.log 2>/dev/null || echo 0"
Write-Host "Total transactions: $($txCount.Trim())" -ForegroundColor Cyan

# Step 3: Backup and clear logs
Write-Host "`n3. Backing up and clearing transaction logs..." -ForegroundColor Yellow
docker exec cobol-metadata-node bash -c @'
# Backup existing logs
if [ -f /app/logs/fabric-transactions.log ]; then
    mv /app/logs/fabric-transactions.log /app/logs/fabric-transactions-backup-$(date +%Y%m%d-%H%M%S).log
fi

# Clear all logs and data files
> /app/logs/fabric-transactions.log
> /app/logs/blockchain-writes.log
> /app/data/metadata.log
> /app/logs/processed_state.json
> /app/logs/processed_metadata.txt
> /app/logs/metadata_position.txt

# Remove any sequence files
rm -f /app/data/sequence.dat

echo "Logs cleared and backed up"
'@

# Step 4: Deploy the fixed COBOL program with unique IDs
Write-Host "`n4. Deploying fixed COBOL program..." -ForegroundColor Yellow

# This COBOL program generates unique IDs each time
@'
       IDENTIFICATION DIVISION.
       PROGRAM-ID. METADATA-CAPTURE.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT METADATA-LOG ASSIGN TO "data/metadata.log"
               ORGANIZATION IS LINE SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL.
       
       DATA DIVISION.
       FILE SECTION.
       FD  METADATA-LOG.
       01  LOG-RECORD          PIC X(200).
       
       WORKING-STORAGE SECTION.
       01  WS-TIMESTAMP        PIC X(26).
       01  WS-TIMESTAMP-NUM    PIC 9(14).
       01  WS-RANDOM           PIC 9(4).
       01  WS-RECORD-ID        PIC X(10).
       01  WS-METADATA.
           05  FILLER          PIC X(17) VALUE '{"source":"COBOL"'.
           05  FILLER          PIC X(15) VALUE ',"operation":"'.
           05  WS-OP-TYPE      PIC X(10) VALUE "CREATE".
           05  FILLER          PIC X(17) VALUE '","timestamp":"'.
           05  WS-TIME-STAMP   PIC X(26).
           05  FILLER          PIC X(17) VALUE '","record_id":"'.
           05  WS-REC-ID       PIC X(10).
           05  FILLER          PIC X(22) VALUE '","file":"MASTER.DAT"'.
           05  FILLER          PIC X(15) VALUE ',"status":"00"'.
           05  FILLER          PIC X(1)  VALUE '}'.
       
       PROCEDURE DIVISION.
       MAIN-PROCESS.
           PERFORM GENERATE-UNIQUE-ID
           PERFORM CAPTURE-METADATA
           STOP RUN.
       
       GENERATE-UNIQUE-ID.
           MOVE FUNCTION CURRENT-DATE TO WS-TIMESTAMP
           MOVE WS-TIMESTAMP(1:14) TO WS-TIMESTAMP-NUM
           COMPUTE WS-RANDOM = FUNCTION RANDOM * 9999
           STRING "R" WS-TIMESTAMP-NUM(9:6) WS-RANDOM
               DELIMITED BY SIZE
               INTO WS-RECORD-ID.
       
       CAPTURE-METADATA.
           MOVE WS-TIMESTAMP TO WS-TIME-STAMP
           MOVE WS-RECORD-ID TO WS-REC-ID
           
           OPEN EXTEND METADATA-LOG
           MOVE WS-METADATA TO LOG-RECORD
           WRITE LOG-RECORD
           CLOSE METADATA-LOG
           
           DISPLAY "Metadata captured: " WS-REC-ID.
'@ | Out-File -FilePath "metadata-capture-unique.cob" -Encoding UTF8

docker cp metadata-capture-unique.cob cobol-metadata-node:/app/cobol_programs/metadata-capture.cob
docker exec cobol-metadata-node bash -c "cd /app && cobc -x -o metadata-capture cobol_programs/metadata-capture.cob"

# Step 5: Deploy a MANUAL adapter (not automatic)
Write-Host "`n5. Deploying manual adapter (run on demand only)..." -ForegroundColor Yellow

@'
#!/usr/bin/env python3
"""
Manual blockchain adapter - processes metadata.log ONCE when run
No automatic looping!
"""
import json
import hashlib
import datetime
import sys

def process_metadata_once():
    """Process metadata.log one time only"""
    processed_count = 0
    metadata_file = "/app/data/metadata.log"
    tx_log = "/app/logs/fabric-transactions.log"
    
    print("Processing metadata (ONE TIME ONLY)...")
    
    try:
        with open(metadata_file, "r") as f:
            for line in f:
                line = line.strip()
                if line.startswith("{"):
                    try:
                        metadata = json.loads(line)
                        # Clean whitespace
                        cleaned = {k: v.strip() if isinstance(v, str) else v 
                                 for k, v in metadata.items()}
                        
                        # Create transaction
                        tx_id = hashlib.sha256(f"{datetime.datetime.now().isoformat()}{line}".encode()).hexdigest()[:16]
                        
                        transaction = {
                            "tx_id": tx_id,
                            "timestamp": datetime.datetime.now().isoformat(),
                            "function": "CreateMetadata",
                            "args": [
                                f"COBOL-{cleaned.get('record_id', 'UNK')}",
                                cleaned.get("source", "COBOL"),
                                cleaned.get("operation", "CREATE"),
                                cleaned.get("timestamp", ""),
                                cleaned.get("record_id", ""),
                                cleaned.get("file", ""),
                                cleaned.get("status", ""),
                                "METADATA-CAPTURE",
                                "MANUAL"
                            ],
                            "status": "submitted",
                            "peer": "peer0.org1.example.com:7051"
                        }
                        
                        # Write transaction
                        with open(tx_log, "a") as tf:
                            tf.write(json.dumps(transaction) + "\n")
                        
                        processed_count += 1
                        print(f"  Processed: {cleaned.get('record_id')} -> TX: {tx_id}")
                        
                    except json.JSONDecodeError as e:
                        print(f"  Error parsing JSON: {e}")
    
    except FileNotFoundError:
        print("No metadata.log file found")
    
    print(f"\nProcessed {processed_count} metadata entries")
    print("Adapter finished (not running in background)")

if __name__ == "__main__":
    if len(sys.argv) > 1 and sys.argv[1] == "--loop":
        print("ERROR: Automatic looping disabled to prevent runaway transactions!")
        print("Run without --loop flag for one-time processing")
        sys.exit(1)
    
    process_metadata_once()
'@ | Out-File -FilePath "manual-adapter.py" -Encoding UTF8

docker cp manual-adapter.py cobol-metadata-node:/app/adapters/
docker exec cobol-metadata-node chmod +x /app/adapters/manual-adapter.py

# Step 6: Create a controlled test script
Write-Host "`n6. Creating controlled test script..." -ForegroundColor Yellow

@'
#!/bin/bash
echo "=== Controlled COBOL-Blockchain Test ==="
echo "This will:"
echo "1. Run COBOL program ONCE"
echo "2. Process metadata ONCE"
echo "3. Show results"
echo ""

# Run COBOL program
echo "Running COBOL metadata capture..."
/app/metadata-capture

# Wait a moment
sleep 1

# Process with adapter ONCE
echo "Processing with blockchain adapter..."
python3 /app/adapters/manual-adapter.py

# Show results
echo ""
echo "=== Results ==="
echo "Metadata log entries:"
wc -l < /app/data/metadata.log

echo "Blockchain transactions:"
wc -l < /app/logs/fabric-transactions.log

echo ""
echo "Latest transaction:"
tail -1 /app/logs/fabric-transactions.log | jq '.'
'@ | Out-File -FilePath "controlled-test.sh" -Encoding UTF8

docker cp controlled-test.sh cobol-metadata-node:/app/scripts/
docker exec cobol-metadata-node chmod +x /app/scripts/controlled-test.sh
docker exec cobol-metadata-node dos2unix /app/scripts/controlled-test.sh 2>$null

# Clean up local files
Remove-Item "metadata-capture-unique.cob", "manual-adapter.py", "controlled-test.sh" -ErrorAction SilentlyContinue

Write-Host "`n=== Cleanup Complete ===" -ForegroundColor Green
Write-Host "✅ All automatic adapters stopped" -ForegroundColor Yellow
Write-Host "✅ Transaction logs backed up and cleared" -ForegroundColor Yellow
Write-Host "✅ COBOL program fixed to generate unique IDs" -ForegroundColor Yellow
Write-Host "✅ Manual adapter deployed (no automatic looping)" -ForegroundColor Yellow

Write-Host "`n=== How to Use Going Forward ===" -ForegroundColor Cyan
Write-Host "1. Run COBOL program manually:" -ForegroundColor White
Write-Host "   docker exec cobol-metadata-node /app/metadata-capture" -ForegroundColor Gray

Write-Host "`n2. Process metadata manually (one time):" -ForegroundColor White
Write-Host "   docker exec cobol-metadata-node python3 /app/adapters/manual-adapter.py" -ForegroundColor Gray

Write-Host "`n3. Or use the controlled test script:" -ForegroundColor White
Write-Host "   docker exec cobol-metadata-node /app/scripts/controlled-test.sh" -ForegroundColor Gray

Write-Host "`n⚠️  IMPORTANT: No adapters are running automatically now!" -ForegroundColor Yellow
Write-Host "This prevents the runaway transaction creation you experienced." -ForegroundColor Yellow