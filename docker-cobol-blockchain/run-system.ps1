Write-Host "=== Direct Container Setup (No Windows Line Ending Issues) ===" -ForegroundColor Green

# Step 1: Clean up
Write-Host "`n1. Cleaning up..." -ForegroundColor Yellow
docker exec cobol-metadata-node bash -c "pkill -f python3 2>/dev/null || true"
docker exec cobol-metadata-node bash -c "rm -rf /app/pipes/* && mkdir -p /app/pipes /app/logs /app/data /app/cobol_programs /app/adapters /app/scripts"
docker exec cobol-metadata-node bash -c "> /app/logs/fabric-transactions.log"

# Step 2: Create COBOL programs directly in container
Write-Host "`n2. Creating COBOL programs in container..." -ForegroundColor Yellow

docker exec cobol-metadata-node bash -c @'
cat > /app/cobol_programs/customer-crud.cob << 'EOFCOB'
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CUSTOMER-CRUD.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CUSTOMER-FILE ASSIGN TO "data/customers.dat"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CUST-ID
               FILE STATUS IS WS-FILE-STATUS.
           
           SELECT EVENT-PIPE ASSIGN TO "pipes/crud-events"
               ORGANIZATION IS LINE SEQUENTIAL.
       
       DATA DIVISION.
       FILE SECTION.
       FD  CUSTOMER-FILE.
       01  CUSTOMER-RECORD.
           05  CUST-ID         PIC X(10).
           05  CUST-NAME       PIC X(30).
           05  CUST-BALANCE    PIC 9(9)V99.
           05  LAST-UPDATED    PIC X(26).
       
       FD  EVENT-PIPE.
       01  EVENT-MESSAGE       PIC X(500).
       
       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS      PIC XX.
       01  WS-OPERATION        PIC X(10).
       01  WS-TIMESTAMP        PIC X(26).
       01  WS-EVENT-JSON       PIC X(500).
       
       LINKAGE SECTION.
       01  LK-OPERATION        PIC X.
       01  LK-CUST-ID          PIC X(10).
       01  LK-CUST-NAME        PIC X(30).
       01  LK-CUST-BALANCE     PIC 9(9)V99.
       
       PROCEDURE DIVISION USING LK-OPERATION LK-CUST-ID 
                                LK-CUST-NAME LK-CUST-BALANCE.
           MOVE FUNCTION CURRENT-DATE TO WS-TIMESTAMP
           
           EVALUATE LK-OPERATION
               WHEN "C" PERFORM CREATE-CUSTOMER
               WHEN "R" PERFORM READ-CUSTOMER
               WHEN "U" PERFORM UPDATE-CUSTOMER
               WHEN "D" PERFORM DELETE-CUSTOMER
           END-EVALUATE
           
           GOBACK.
       
       CREATE-CUSTOMER.
           OPEN I-O CUSTOMER-FILE
           MOVE LK-CUST-ID TO CUST-ID
           MOVE LK-CUST-NAME TO CUST-NAME
           MOVE LK-CUST-BALANCE TO CUST-BALANCE
           MOVE WS-TIMESTAMP TO LAST-UPDATED
           WRITE CUSTOMER-RECORD
           IF WS-FILE-STATUS = "00"
               MOVE "CREATE" TO WS-OPERATION
               PERFORM SEND-EVENT
               DISPLAY "Customer created: " CUST-ID
           END-IF
           CLOSE CUSTOMER-FILE.
       
       UPDATE-CUSTOMER.
           OPEN I-O CUSTOMER-FILE
           MOVE LK-CUST-ID TO CUST-ID
           READ CUSTOMER-FILE
           IF WS-FILE-STATUS = "00"
               MOVE LK-CUST-NAME TO CUST-NAME
               MOVE LK-CUST-BALANCE TO CUST-BALANCE
               MOVE WS-TIMESTAMP TO LAST-UPDATED
               REWRITE CUSTOMER-RECORD
               MOVE "UPDATE" TO WS-OPERATION
               PERFORM SEND-EVENT
               DISPLAY "Customer updated: " CUST-ID
           END-IF
           CLOSE CUSTOMER-FILE.
       
       READ-CUSTOMER.
           OPEN INPUT CUSTOMER-FILE
           MOVE LK-CUST-ID TO CUST-ID
           READ CUSTOMER-FILE
           IF WS-FILE-STATUS = "00"
               MOVE CUST-NAME TO LK-CUST-NAME
               MOVE CUST-BALANCE TO LK-CUST-BALANCE
               MOVE "READ" TO WS-OPERATION
               PERFORM SEND-EVENT
           END-IF
           CLOSE CUSTOMER-FILE.
       
       DELETE-CUSTOMER.
           OPEN I-O CUSTOMER-FILE
           MOVE LK-CUST-ID TO CUST-ID
           DELETE CUSTOMER-FILE RECORD
           IF WS-FILE-STATUS = "00"
               MOVE "DELETE" TO WS-OPERATION
               PERFORM SEND-EVENT
               DISPLAY "Customer deleted: " CUST-ID
           END-IF
           CLOSE CUSTOMER-FILE.
       
       SEND-EVENT.
           STRING "{" DELIMITED BY SIZE
                  "\"operation\":\"" DELIMITED BY SIZE
                  WS-OPERATION DELIMITED BY SPACE
                  "\",\"timestamp\":\"" DELIMITED BY SIZE
                  WS-TIMESTAMP DELIMITED BY SPACE
                  "\",\"entity\":\"CUSTOMER\"" DELIMITED BY SIZE
                  ",\"id\":\"" DELIMITED BY SIZE
                  CUST-ID DELIMITED BY SPACE
                  "\",\"name\":\"" DELIMITED BY SIZE
                  CUST-NAME DELIMITED BY SPACE
                  "\",\"balance\":" DELIMITED BY SIZE
                  CUST-BALANCE DELIMITED BY SIZE
                  ",\"status\":\"" DELIMITED BY SIZE
                  WS-FILE-STATUS DELIMITED BY SIZE
                  "\"}" DELIMITED BY SIZE
                  INTO WS-EVENT-JSON
           
           OPEN OUTPUT EVENT-PIPE
           MOVE WS-EVENT-JSON TO EVENT-MESSAGE
           WRITE EVENT-MESSAGE
           CLOSE EVENT-PIPE.
EOFCOB
'@

docker exec cobol-metadata-node bash -c @'
cat > /app/cobol_programs/test-crud.cob << 'EOFTEST'
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST-CRUD.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-OPERATION        PIC X.
       01  WS-CUST-ID          PIC X(10).
       01  WS-CUST-NAME        PIC X(30).
       01  WS-CUST-BALANCE     PIC 9(9)V99.
       
       PROCEDURE DIVISION.
           DISPLAY "Testing CRUD operations..."
           
      *    Create customer
           MOVE "C" TO WS-OPERATION
           MOVE "CUST001" TO WS-CUST-ID
           MOVE "John Doe" TO WS-CUST-NAME
           MOVE 1000.50 TO WS-CUST-BALANCE
           CALL "CUSTOMER-CRUD" USING WS-OPERATION WS-CUST-ID 
                                     WS-CUST-NAME WS-CUST-BALANCE
           
      *    Update customer
           MOVE "U" TO WS-OPERATION
           MOVE "John Doe Updated" TO WS-CUST-NAME
           MOVE 2500.75 TO WS-CUST-BALANCE
           CALL "CUSTOMER-CRUD" USING WS-OPERATION WS-CUST-ID 
                                     WS-CUST-NAME WS-CUST-BALANCE
           
      *    Read customer
           MOVE "R" TO WS-OPERATION
           CALL "CUSTOMER-CRUD" USING WS-OPERATION WS-CUST-ID 
                                     WS-CUST-NAME WS-CUST-BALANCE
           
           DISPLAY "CRUD tests completed"
           STOP RUN.
EOFTEST
'@

# Step 3: Compile COBOL programs
Write-Host "`n3. Compiling COBOL programs..." -ForegroundColor Yellow

docker exec cobol-metadata-node bash -c @'
cd /app
# Compile customer-crud as a dynamic module
cobc -m -o customer-crud.so cobol_programs/customer-crud.cob
if [ $? -eq 0 ]; then
    echo "customer-crud.so compiled successfully"
    export COB_LIBRARY_PATH=/app
    # Compile test program linking to the module
    cobc -x -o test-crud cobol_programs/test-crud.cob -L/app -lcustomer-crud
    if [ $? -eq 0 ]; then
        echo "test-crud compiled successfully"
        ls -la *.so test-crud
    else
        echo "Failed to compile test-crud"
    fi
else
    echo "Failed to compile customer-crud.so"
fi
'@

# Step 4: Create Python adapter
Write-Host "`n4. Creating Python adapter..." -ForegroundColor Yellow

docker exec cobol-metadata-node bash -c @'
cat > /app/adapters/pipe-monitor.py << 'EOFPY'
#!/usr/bin/env python3
import json
import os
import time
import hashlib
import datetime
import errno

def setup_pipe(pipe_path):
    """Create named pipe if it doesn't exist"""
    pipe_dir = os.path.dirname(pipe_path)
    os.makedirs(pipe_dir, exist_ok=True)
    
    try:
        if os.path.exists(pipe_path):
            os.remove(pipe_path)
        os.mkfifo(pipe_path)
        print(f"[SETUP] Created named pipe: {pipe_path}")
    except Exception as e:
        print(f"[ERROR] Creating pipe: {e}")

def monitor_pipe():
    """Monitor named pipe for CRUD events"""
    pipe_path = "/app/pipes/crud-events"
    setup_pipe(pipe_path)
    
    processed_events = set()
    
    print("[MONITOR] Starting pipe monitor...")
    
    while True:
        try:
            # Open pipe for reading (blocks until writer connects)
            with open(pipe_path, "r") as pipe:
                for line in pipe:
                    line = line.strip()
                    if line:
                        process_event(line, processed_events)
        except IOError as e:
            if e.errno == errno.EPIPE:
                print("[INFO] Pipe closed, reopening...")
            else:
                print(f"[ERROR] Reading pipe: {e}")
            time.sleep(0.1)
        except Exception as e:
            print(f"[ERROR] Monitor error: {e}")
            time.sleep(1)

def process_event(event_line, processed_events):
    """Process a single CRUD event"""
    try:
        event_data = json.loads(event_line)
        event_hash = hashlib.sha256(event_line.encode()).hexdigest()
        
        # Skip if already processed
        if event_hash in processed_events:
            print(f"[SKIP] Duplicate event")
            return
        
        processed_events.add(event_hash)
        
        # Generate transaction ID
        tx_id = hashlib.sha256(
            f"{datetime.datetime.now().isoformat()}{event_line}".encode()
        ).hexdigest()[:16]
        
        # Create blockchain transaction
        tx = {
            "tx_id": tx_id,
            "timestamp": datetime.datetime.now().isoformat(),
            "function": "RecordCRUDOperation",
            "args": [
                tx_id,
                event_data.get("operation", "UNKNOWN"),
                event_data.get("entity", ""),
                event_data.get("id", ""),
                event_data.get("timestamp", ""),
                json.dumps(event_data),
                "CUSTOMER-CRUD",
                "PIPE-MONITOR"
            ],
            "status": "submitted",
            "peer": "peer0.org1.example.com:7051"
        }
        
        # Write to blockchain log
        with open("/app/logs/fabric-transactions.log", "a") as f:
            f.write(json.dumps(tx) + "\n")
        
        print(f"[EVENT] {event_data['operation']} {event_data.get('id', 'N/A')} -> TX: {tx_id}")
        
    except json.JSONDecodeError as e:
        print(f"[ERROR] Invalid JSON: {e}")
    except Exception as e:
        print(f"[ERROR] Processing event: {e}")

if __name__ == "__main__":
    monitor_pipe()
EOFPY

chmod +x /app/adapters/pipe-monitor.py
'@

# Step 5: Create management scripts
Write-Host "`n5. Creating management scripts..." -ForegroundColor Yellow

docker exec cobol-metadata-node bash -c @'
cat > /app/scripts/start-system.sh << 'EOFSTART'
#!/bin/bash
echo "Starting COBOL-Blockchain Event System..."
pkill -f "pipe-monitor.py" 2>/dev/null
mkdir -p /app/pipes /app/logs
export COB_LIBRARY_PATH=/app
python3 /app/adapters/pipe-monitor.py > /app/logs/adapter.log 2>&1 &
echo $! > /app/adapter.pid
echo "Adapter started with PID: $(cat /app/adapter.pid)"
sleep 2
tail -10 /app/logs/adapter.log
EOFSTART

cat > /app/scripts/stop-system.sh << 'EOFSTOP'
#!/bin/bash
if [ -f /app/adapter.pid ]; then
    PID=$(cat /app/adapter.pid)
    kill $PID 2>/dev/null
    rm /app/adapter.pid
    echo "Adapter stopped (PID: $PID)"
else
    pkill -f "pipe-monitor.py" 2>/dev/null
    echo "Stopped all pipe monitors"
fi
EOFSTOP

cat > /app/scripts/status.sh << 'EOFSTATUS'
#!/bin/bash
echo "=== System Status ==="
if [ -f /app/adapter.pid ] && ps -p $(cat /app/adapter.pid) > /dev/null 2>&1; then
    echo "Adapter: RUNNING (PID: $(cat /app/adapter.pid))"
else
    echo "Adapter: STOPPED"
fi
echo ""
echo "Transaction count: $(wc -l < /app/logs/fabric-transactions.log 2>/dev/null || echo 0)"
if [ -f /app/logs/fabric-transactions.log ]; then
    echo ""
    echo "Latest transactions:"
    tail -3 /app/logs/fabric-transactions.log | while read line; do
        echo "$line" | python3 -c "import sys, json; d=json.loads(sys.stdin.read()); print(f\"{d['tx_id']} | {d['args'][1]} | {d['args'][3]}\")" 2>/dev/null || echo "$line"
    done
fi
EOFSTATUS

chmod +x /app/scripts/*.sh
'@

# Step 6: Start the system
Write-Host "`n6. Starting the system..." -ForegroundColor Cyan
docker exec cobol-metadata-node /app/scripts/start-system.sh

Start-Sleep -Seconds 3

# Step 7: Test the system
Write-Host "`n7. Running CRUD test..." -ForegroundColor Cyan
docker exec cobol-metadata-node bash -c "export COB_LIBRARY_PATH=/app && /app/test-crud"

Start-Sleep -Seconds 2

# Step 8: Show status
Write-Host "`n8. System Status:" -ForegroundColor Cyan
docker exec cobol-metadata-node /app/scripts/status.sh

Write-Host "`n✅ System deployed and running!" -ForegroundColor Green
Write-Host "`nUseful commands:" -ForegroundColor Yellow
Write-Host "  Status:     docker exec cobol-metadata-node /app/scripts/status.sh" -ForegroundColor Gray
Write-Host "  Logs:       docker exec cobol-metadata-node tail -f /app/logs/adapter.log" -ForegroundColor Gray
Write-Host "  TXs:        docker exec cobol-metadata-node tail -f /app/logs/fabric-transactions.log" -ForegroundColor Gray
Write-Host "  Stop:       docker exec cobol-metadata-node /app/scripts/stop-system.sh" -ForegroundColor Gray
Write-Host "  Test again: docker exec cobol-metadata-node bash -c 'export COB_LIBRARY_PATH=/app && /app/test-crud'" -ForegroundColor Gray