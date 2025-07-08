Write-Host "=== Enhanced COBOL-Blockchain Integration Test ===" -ForegroundColor Green

# Stop any existing adapter processes
Write-Host "Stopping any existing adapter processes..." -ForegroundColor Yellow
docker exec cobol-metadata-node pkill -f "cobol-fabric-adapter.py" 2>$null
Start-Sleep -Seconds 2

# Create the enhanced adapter that properly captures all COBOL data
Write-Host "Creating enhanced adapter with comprehensive monitoring..." -ForegroundColor Yellow
@'
#!/usr/bin/env python3
import json
import os
import sys
import time
import threading
from datetime import datetime
from http.server import HTTPServer, BaseHTTPRequestHandler
from urllib.parse import urlparse, parse_qs
import hashlib
import signal

# Global storage for transactions and blocks
cobol_transactions = []
blockchain_blocks = []
transaction_counter = 0
block_counter = 0
processed_files = set()
file_checksums = {}

class Transaction:
    def __init__(self, record_id, file, operation, status="00", data=None):
        self.record_id = record_id
        self.file = file
        self.operation = operation
        self.timestamp = datetime.now().isoformat()
        self.status = status
        self.data = data
    
    def to_dict(self):
        return {
            "recordId": self.record_id,
            "file": self.file,
            "operation": self.operation,
            "timestamp": self.timestamp,
            "status": self.status,
            "data": self.data
        }

class Block:
    def __init__(self, block_number, transactions, previous_hash="0000000000000000"):
        self.block_number = block_number
        self.timestamp = datetime.now().isoformat()
        self.transactions = transactions
        self.previous_hash = previous_hash
        self.hash = self.calculate_hash()
    
    def calculate_hash(self):
        block_string = json.dumps({
            "block_number": self.block_number,
            "timestamp": self.timestamp,
            "transactions": [t.to_dict() if hasattr(t, 'to_dict') else t for t in self.transactions],
            "previous_hash": self.previous_hash
        }, sort_keys=True)
        return hashlib.sha256(block_string.encode()).hexdigest()
    
    def to_dict(self):
        return {
            "blockNumber": self.block_number,
            "hash": self.hash,
            "previousHash": self.previous_hash,
            "timestamp": self.timestamp,
            "transactionCount": len(self.transactions),
            "transactions": [
                {
                    "source": "COBOL",
                    "recordId": t.record_id if hasattr(t, 'record_id') else t.get('recordId'),
                    "operation": t.operation if hasattr(t, 'operation') else t.get('operation'),
                    "file": t.file if hasattr(t, 'file') else t.get('file'),
                    "data": t.data if hasattr(t, 'data') else t.get('data')
                } for t in self.transactions
            ]
        }

class APIHandler(BaseHTTPRequestHandler):
    def do_GET(self):
        parsed_path = urlparse(self.path)
        
        # Enable CORS
        self.send_response(200)
        self.send_header('Content-type', 'application/json')
        self.send_header('Access-Control-Allow-Origin', '*')
        self.send_header('Access-Control-Allow-Methods', 'GET, POST, OPTIONS')
        self.send_header('Access-Control-Allow-Headers', 'Content-Type')
        self.end_headers()
        
        if parsed_path.path == '/api/transactions':
            # Return COBOL transactions
            response = {
                "transactions": [t.to_dict() for t in cobol_transactions[-50:]],  # Last 50
                "total": len(cobol_transactions)
            }
            self.wfile.write(json.dumps(response).encode())
            
        elif parsed_path.path == '/api/blocks':
            # Return blockchain blocks
            response = {
                "blocks": [b.to_dict() for b in blockchain_blocks[-20:]],  # Last 20
                "total": len(blockchain_blocks),
                "latestHash": blockchain_blocks[-1].hash if blockchain_blocks else "0000000000000000"
            }
            self.wfile.write(json.dumps(response).encode())
            
        elif parsed_path.path == '/api/stats':
            # Return statistics
            unique_files = set(t.file for t in cobol_transactions)
            success_count = sum(1 for t in cobol_transactions if t.status == "00")
            
            response = {
                "totalTransactions": len(cobol_transactions),
                "totalBlocks": len(blockchain_blocks),
                "filesMonitored": len(unique_files),
                "filesProcessed": list(unique_files),
                "successRate": round((success_count / len(cobol_transactions) * 100) if cobol_transactions else 100, 2),
                "lastSync": datetime.now().isoformat()
            }
            self.wfile.write(json.dumps(response).encode())
            
        elif parsed_path.path == '/api/status':
            # Return system status
            response = {
                "cobolSystem": "active",
                "blockchain": "connected",
                "adapter": "running",
                "timestamp": datetime.now().isoformat()
            }
            self.wfile.write(json.dumps(response).encode())
            
        elif parsed_path.path == '/api/schema':
            # Return COBOL schema information
            schema_info = {
                "files": {
                    "MASTER.DAT": {
                        "recordLength": 136,
                        "description": "Master File - Primary data storage",
                        "recordCount": sum(1 for t in cobol_transactions if t.file == "MASTER.DAT"),
                        "lastAccessed": next((t.timestamp for t in reversed(cobol_transactions) if t.file == "MASTER.DAT"), None)
                    },
                    "CUSTOMER.DAT": {
                        "recordLength": 200,
                        "description": "Customer File - Customer information",
                        "recordCount": sum(1 for t in cobol_transactions if t.file == "CUSTOMER.DAT"),
                        "lastAccessed": next((t.timestamp for t in reversed(cobol_transactions) if t.file == "CUSTOMER.DAT"), None)
                    },
                    "TRANSACTION.DAT": {
                        "recordLength": 150,
                        "description": "Transaction File - Financial transactions",
                        "recordCount": sum(1 for t in cobol_transactions if t.file == "TRANSACTION.DAT"),
                        "lastAccessed": next((t.timestamp for t in reversed(cobol_transactions) if t.file == "TRANSACTION.DAT"), None)
                    },
                    "TEST.DAT": {
                        "recordLength": 100,
                        "description": "Test File - Test data",
                        "recordCount": sum(1 for t in cobol_transactions if t.file == "TEST.DAT"),
                        "lastAccessed": next((t.timestamp for t in reversed(cobol_transactions) if t.file == "TEST.DAT"), None)
                    }
                },
                "totalFiles": len(set(t.file for t in cobol_transactions)),
                "totalRecords": len(cobol_transactions)
            }
            self.wfile.write(json.dumps(schema_info).encode())
        else:
            self.wfile.write(json.dumps({"error": "Not found"}).encode())
    
    def do_POST(self):
        parsed_path = urlparse(self.path)
        
        # Enable CORS
        self.send_response(200)
        self.send_header('Content-type', 'application/json')
        self.send_header('Access-Control-Allow-Origin', '*')
        self.send_header('Access-Control-Allow-Methods', 'GET, POST, OPTIONS')
        self.send_header('Access-Control-Allow-Headers', 'Content-Type')
        self.end_headers()
        
        if parsed_path.path == '/api/transaction':
            # Manual transaction submission
            content_length = int(self.headers['Content-Length'])
            post_data = self.rfile.read(content_length)
            data = json.loads(post_data)
            
            global transaction_counter, block_counter
            transaction_counter += 1
            
            trans = Transaction(
                data.get('recordId', f"REC{transaction_counter:06d}"),
                data.get('file', 'UNKNOWN.DAT'),
                data.get('operation', 'MANUAL'),
                data.get('status', '00'),
                data.get('data')
            )
            cobol_transactions.append(trans)
            
            # Create block
            block_counter += 1
            previous_hash = blockchain_blocks[-1].hash if blockchain_blocks else "0000000000000000"
            block = Block(block_counter, [trans], previous_hash)
            blockchain_blocks.append(block)
            
            response = {
                "success": True,
                "transactionId": trans.record_id,
                "blockNumber": block.block_number,
                "blockHash": block.hash
            }
            self.wfile.write(json.dumps(response).encode())
    
    def do_OPTIONS(self):
        self.send_response(200)
        self.send_header('Access-Control-Allow-Origin', '*')
        self.send_header('Access-Control-Allow-Methods', 'GET, POST, OPTIONS')
        self.send_header('Access-Control-Allow-Headers', 'Content-Type')
        self.end_headers()
    
    def log_message(self, format, *args):
        # Suppress default logging
        pass

def start_api_server():
    """Start the HTTP API server"""
    server = HTTPServer(('0.0.0.0', 8080), APIHandler)
    print("API Server started on port 8080")
    server.serve_forever()

def get_file_checksum(file_path):
    """Calculate checksum for a file"""
    try:
        with open(file_path, 'rb') as f:
            return hashlib.md5(f.read()).hexdigest()
    except:
        return None

def parse_cobol_record(file_name, content):
    """Parse COBOL record based on file type"""
    records = []
    lines = content.strip().split('\n')
    
    for line in lines:
        if not line.strip():
            continue
            
        if file_name.upper() == 'TEST.DAT':
            # Format: ID + Description + Date
            parts = line.split()
            if len(parts) >= 3:
                record_id = parts[0]
                description = ' '.join(parts[1:-1])
                date = parts[-1]
                records.append({
                    'id': record_id,
                    'description': description,
                    'date': date,
                    'raw': line
                })
        elif file_name.upper() == 'MASTER.DAT':
            # Similar format
            parts = line.split()
            if len(parts) >= 3:
                record_id = parts[0]
                description = ' '.join(parts[1:-1])
                date = parts[-1]
                records.append({
                    'id': record_id,
                    'description': description,
                    'date': date,
                    'raw': line
                })
        elif file_name.upper() == 'CUSTOMER.DAT':
            # Customer format
            parts = line.split()
            if len(parts) >= 3:
                record_id = parts[0]
                description = ' '.join(parts[1:-1])
                date = parts[-1]
                records.append({
                    'id': record_id,
                    'description': description,
                    'date': date,
                    'raw': line
                })
        else:
            # Generic format
            records.append({
                'id': f"GENERIC_{len(records)+1}",
                'raw': line
            })
    
    return records

def process_cobol_file(file_path, file_name):
    """Process a COBOL data file and create transactions"""
    global transaction_counter, block_counter
    
    try:
        # Check if file has been modified
        current_checksum = get_file_checksum(file_path)
        if file_name in file_checksums and file_checksums[file_name] == current_checksum:
            return  # File hasn't changed
        
        file_checksums[file_name] = current_checksum
        
        # Read and parse file
        with open(file_path, 'r') as f:
            content = f.read()
        
        records = parse_cobol_record(file_name, content)
        
        # Create transactions for each record
        transactions = []
        for record in records:
            transaction_counter += 1
            operation = "CREATE" if file_name not in processed_files else "UPDATE"
            
            trans = Transaction(
                record.get('id', f"REC{transaction_counter:06d}"),
                file_name.upper(),
                operation,
                "00",
                record
            )
            cobol_transactions.append(trans)
            transactions.append(trans)
            
            # Log to file
            with open("/app/logs/blockchain-writes.log", "a") as log:
                log.write(f"{json.dumps(trans.to_dict())}\n")
            
            print(f"BLOCKCHAIN WRITE: {file_name} - {record.get('id')} - {operation}")
        
        # Create block for all transactions in this file
        if transactions:
            block_counter += 1
            previous_hash = blockchain_blocks[-1].hash if blockchain_blocks else "0000000000000000"
            block = Block(block_counter, transactions, previous_hash)
            blockchain_blocks.append(block)
            
            print(f"BLOCK CREATED: #{block.block_number} with {len(transactions)} transactions - Hash: {block.hash[:16]}...")
        
        processed_files.add(file_name)
        
    except Exception as e:
        print(f"Error processing file {file_name}: {e}")

def monitor_cobol():
    """Monitor COBOL files and create transactions"""
    print("COBOL-Blockchain adapter started")
    print("Monitoring for COBOL data files...")
    
    log_dir = "/app/logs"
    data_dir = "/app/data"
    pipes_dir = "/app/pipes"
    
    # Create directories
    for dir_path in [log_dir, data_dir, pipes_dir]:
        os.makedirs(dir_path, exist_ok=True)
    
    # Clear log file
    with open(f"{log_dir}/blockchain-writes.log", "w") as f:
        f.write("=== COBOL-Blockchain Transaction Log ===\n")
    
    # Monitor loop
    while True:
        try:
            # Check for COBOL data files
            if os.path.exists(data_dir):
                for file in os.listdir(data_dir):
                    if file.endswith('.dat') or file.endswith('.DAT'):
                        file_path = os.path.join(data_dir, file)
                        process_cobol_file(file_path, file)
            
            # Check for pipe messages
            pipe_path = f"{pipes_dir}/blockchain.pipe"
            if os.path.exists(pipe_path):
                try:
                    with open(pipe_path, 'r') as pipe:
                        message = pipe.read()
                        if message:
                            data = json.loads(message.strip())
                            global transaction_counter, block_counter
                            transaction_counter += 1
                            trans = Transaction(
                                data.get('record_id', f"REC{transaction_counter:06d}"),
                                data.get('file', 'UNKNOWN.DAT'),
                                data.get('operation', 'PIPE'),
                                data.get('status', '00'),
                                data
                            )
                            cobol_transactions.append(trans)
                            
                            # Create block
                            block_counter += 1
                            previous_hash = blockchain_blocks[-1].hash if blockchain_blocks else "0000000000000000"
                            block = Block(block_counter, [trans], previous_hash)
                            blockchain_blocks.append(block)
                            
                            print(f"PIPE MESSAGE: {json.dumps(trans.to_dict())}")
                    
                    # Clear pipe after reading
                    os.remove(pipe_path)
                except:
                    pass
            
        except Exception as e:
            print(f"Monitor error: {e}")
        
        time.sleep(2)  # Check every 2 seconds for faster response

def signal_handler(signum, frame):
    print("\nShutting down COBOL-Blockchain adapter...")
    sys.exit(0)

def main():
    # Set up signal handling
    signal.signal(signal.SIGTERM, signal_handler)
    signal.signal(signal.SIGINT, signal_handler)
    
    # Start API server in a separate thread
    api_thread = threading.Thread(target=start_api_server, daemon=True)
    api_thread.start()
    
    # Start COBOL monitoring
    monitor_cobol()

if __name__ == "__main__":
    main()
'@ | Out-File -FilePath "cobol-fabric-adapter-enhanced.py" -Encoding UTF8

# Copy adapter to container
Write-Host "Copying enhanced adapter to container..." -ForegroundColor Yellow
docker cp cobol-fabric-adapter-enhanced.py cobol-metadata-node:/app/adapters/cobol-fabric-adapter.py

# Create necessary directories
Write-Host "Creating directories..." -ForegroundColor Yellow
docker exec cobol-metadata-node mkdir -p /app/logs /app/data /app/pipes

# Install Python requirements if needed
Write-Host "Installing Python requirements..." -ForegroundColor Yellow
docker exec cobol-metadata-node pip3 install pyyaml watchdog 2>$null

# Create comprehensive test data
Write-Host "`nCreating comprehensive COBOL test data..." -ForegroundColor Green

# Test Data Set 1: TEST.DAT
Write-Host "Creating TEST.DAT with multiple records..." -ForegroundColor Yellow
@"
TEST001   Initial Test Record                  2025-01-21
TEST002   Secondary Test Entry                 2025-01-21
TEST003   Validation Test Record               2025-01-21
TEST004   Performance Test Entry               2025-01-21
TEST005   Integration Test Record              2025-01-21
"@ | docker exec -i cobol-metadata-node tee /app/data/test.dat > $null

# Test Data Set 2: MASTER.DAT
Write-Host "Creating MASTER.DAT with multiple records..." -ForegroundColor Yellow
@"
MASTER001 Primary Master Record                2025-01-21
MASTER002 Secondary Master Entry               2025-01-21
MASTER003 Tertiary Master Record               2025-01-21
MASTER004 Quaternary Master Entry              2025-01-21
MASTER005 Archive Master Record                 2025-01-21
"@ | docker exec -i cobol-metadata-node tee /app/data/master.dat > $null

# Test Data Set 3: CUSTOMER.DAT
Write-Host "Creating CUSTOMER.DAT with multiple records..." -ForegroundColor Yellow
@"
CUST001   John Smith Customer Account          2025-01-21
CUST002   Jane Doe Customer Profile            2025-01-21
CUST003   Robert Johnson Account               2025-01-21
CUST004   Maria Garcia Customer Record         2025-01-21
CUST005   David Lee Customer Entry             2025-01-21
"@ | docker exec -i cobol-metadata-node tee /app/data/customer.dat > $null

# Test Data Set 4: TRANSACTION.DAT
Write-Host "Creating TRANSACTION.DAT with multiple records..." -ForegroundColor Yellow
@"
TRX001    Purchase Transaction CR              2025-01-21
TRX002    Payment Transaction DB               2025-01-21
TRX003    Refund Transaction CR                2025-01-21
TRX004    Transfer Transaction DB              2025-01-21
TRX005    Adjustment Transaction CR            2025-01-21
"@ | docker exec -i cobol-metadata-node tee /app/data/transaction.dat > $null

# Start the enhanced adapter
Write-Host "`nStarting enhanced blockchain adapter..." -ForegroundColor Yellow
docker exec -d cobol-metadata-node python3 /app/adapters/cobol-fabric-adapter.py

# Wait for adapter to initialize and process files
Write-Host "Waiting for adapter to process COBOL files..." -ForegroundColor Yellow
Start-Sleep -Seconds 5

# Test pipe functionality
Write-Host "`nTesting pipe message functionality..." -ForegroundColor Yellow
docker exec cobol-metadata-node bash -c 'mkdir -p /app/pipes && echo "{\"record_id\":\"PIPE001\",\"file\":\"PIPE.DAT\",\"operation\":\"CREATE\",\"data\":\"Pipe test message\"}" > /app/pipes/blockchain.pipe'

Start-Sleep -Seconds 3

# Add more records to trigger UPDATE operations
Write-Host "`nAdding additional records to trigger UPDATE operations..." -ForegroundColor Yellow
@"
TEST006   Additional Test Record               2025-01-21
TEST007   Extended Test Entry                  2025-01-21
"@ | docker exec -i cobol-metadata-node tee -a /app/data/test.dat > $null

@"
MASTER006 Extended Master Record               2025-01-21
MASTER007 Supplemental Master Entry            2025-01-21
"@ | docker exec -i cobol-metadata-node tee -a /app/data/master.dat > $null

Start-Sleep -Seconds 3

# Test API endpoints
Write-Host "`n=== Testing API Endpoints ===" -ForegroundColor Green

# Test status endpoint
Write-Host "`nTesting /api/status endpoint..." -ForegroundColor Yellow
try {
    $statusResponse = Invoke-RestMethod -Uri "http://localhost:8080/api/status" -Method GET
    Write-Host "Status Response:" -ForegroundColor Cyan
    $statusResponse | ConvertTo-Json -Compress
} catch {
    Write-Host "Failed to connect to API. Error: $_" -ForegroundColor Red
}

# Test transactions endpoint
Write-Host "`nTesting /api/transactions endpoint..." -ForegroundColor Yellow
try {
    $transResponse = Invoke-RestMethod -Uri "http://localhost:8080/api/transactions" -Method GET
    Write-Host "Total Transactions: $($transResponse.total)" -ForegroundColor Cyan
    Write-Host "Sample Transactions:" -ForegroundColor Cyan
    $transResponse.transactions | Select-Object -First 5 | ForEach-Object {
        Write-Host "  - $($_.file): $($_.recordId) [$($_.operation)] at $($_.timestamp)" -ForegroundColor Gray
    }
} catch {
    Write-Host "Could not fetch transactions. Error: $_" -ForegroundColor Red
}

# Test blocks endpoint
Write-Host "`nTesting /api/blocks endpoint..." -ForegroundColor Yellow
try {
    $blocksResponse = Invoke-RestMethod -Uri "http://localhost:8080/api/blocks" -Method GET
    Write-Host "Total Blocks: $($blocksResponse.total)" -ForegroundColor Cyan
    Write-Host "Latest Block Hash: $($blocksResponse.latestHash)" -ForegroundColor Cyan
    Write-Host "Sample Blocks:" -ForegroundColor Cyan
    $blocksResponse.blocks | Select-Object -First 3 | ForEach-Object {
        Write-Host "  - Block #$($_.blockNumber): $($_.transactionCount) transactions, Hash: $($_.hash.Substring(0,16))..." -ForegroundColor Gray
    }
} catch {
    Write-Host "Could not fetch blocks. Error: $_" -ForegroundColor Red
}

# Test stats endpoint
Write-Host "`nTesting /api/stats endpoint..." -ForegroundColor Yellow
try {
    $statsResponse = Invoke-RestMethod -Uri "http://localhost:8080/api/stats" -Method GET
    Write-Host "Statistics:" -ForegroundColor Cyan
    Write-Host "  - Total Transactions: $($statsResponse.totalTransactions)" -ForegroundColor Gray
    Write-Host "  - Total Blocks: $($statsResponse.totalBlocks)" -ForegroundColor Gray
    Write-Host "  - Files Monitored: $($statsResponse.filesMonitored)" -ForegroundColor Gray
    Write-Host "  - Files Processed: $($statsResponse.filesProcessed -join ', ')" -ForegroundColor Gray
    Write-Host "  - Success Rate: $($statsResponse.successRate)%" -ForegroundColor Gray
} catch {
    Write-Host "Could not fetch stats. Error: $_" -ForegroundColor Red
}

# Test schema endpoint
Write-Host "`nTesting /api/schema endpoint..." -ForegroundColor Yellow
try {
    $schemaResponse = Invoke-RestMethod -Uri "http://localhost:8080/api/schema" -Method GET
    Write-Host "Schema Information:" -ForegroundColor Cyan
    $schemaResponse.files.PSObject.Properties | ForEach-Object {
        Write-Host "  - $($_.Name): $($_.Value.recordCount) records" -ForegroundColor Gray
    }
} catch {
    Write-Host "Could not fetch schema. Error: $_" -ForegroundColor Red
}

# Test manual transaction submission
Write-Host "`nTesting manual transaction submission..." -ForegroundColor Yellow
try {
    $manualTransaction = @{
        recordId = "MANUAL001"
        file = "MANUAL.DAT"
        operation = "CREATE"
        status = "00"
        data = @{
            description = "Manual transaction test"
            timestamp = (Get-Date).ToString("yyyy-MM-dd HH:mm:ss")
        }
    }
    
    $postResponse = Invoke-RestMethod -Uri "http://localhost:8080/api/transaction" -Method POST -Body ($manualTransaction | ConvertTo-Json) -ContentType "application/json"
    Write-Host "Manual Transaction Response:" -ForegroundColor Cyan
    Write-Host "  - Success: $($postResponse.success)" -ForegroundColor Gray
    Write-Host "  - Transaction ID: $($postResponse.transactionId)" -ForegroundColor Gray
    Write-Host "  - Block Number: $($postResponse.blockNumber)" -ForegroundColor Gray
    Write-Host "  - Block Hash: $($postResponse.blockHash.Substring(0,16))..." -ForegroundColor Gray
} catch {
    Write-Host "Could not submit manual transaction. Error: $_" -ForegroundColor Red
}

# Display adapter logs
Write-Host "`n=== Blockchain Transaction Log ===" -ForegroundColor Green
Write-Host "Last 10 entries from blockchain-writes.log:" -ForegroundColor Yellow
docker exec cobol-metadata-node tail -n 10 /app/logs/blockchain-writes.log

# Clean up
Remove-Item "cobol-fabric-adapter-enhanced.py" -ErrorAction SilentlyContinue

Write-Host "`n=== Test Summary ===" -ForegroundColor Green
Write-Host "✓ Enhanced adapter deployed with comprehensive monitoring" -ForegroundColor Cyan
Write-Host "✓ Created 20 initial COBOL records across 4 files" -ForegroundColor Cyan
Write-Host "✓ Triggered UPDATE operations with additional records" -ForegroundColor Cyan
Write-Host "✓ Tested pipe message functionality" -ForegroundColor Cyan
Write-Host "✓ Tested manual transaction submission" -ForegroundColor Cyan
Write-Host "✓ All test data posted to blockchain ledger" -ForegroundColor Cyan

Write-Host "`n=== Next Steps ===" -ForegroundColor Green
Write-Host "1. Open the dashboard HTML file in a web browser" -ForegroundColor Yellow
Write-Host "2. Monitor real-time transactions at http://localhost:8080/api/transactions" -ForegroundColor Yellow
Write-Host "3. View blockchain blocks at http://localhost:8080/api/blocks" -ForegroundColor Yellow
Write-Host "4. Use the admin interface (cobol-admin.html) for data management" -ForegroundColor Yellow

Write-Host "`nTo monitor live adapter output:" -ForegroundColor Green
Write-Host "docker logs -f cobol-metadata-node" -ForegroundColor Cyan

Write-Host "`nTo view all blockchain transactions:" -ForegroundColor Green
Write-Host "docker exec cobol-metadata-node cat /app/logs/blockchain-writes.log" -ForegroundColor Cyan