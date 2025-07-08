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
import re

# Global storage for transactions and blocks
cobol_transactions = []
blockchain_blocks = []
transaction_counter = 0
block_counter = 0

class Transaction:
    def __init__(self, record_id, file, operation, status="00", source="COBOL"):
        self.record_id = record_id.strip()
        self.file = file.strip()
        self.operation = operation.strip()
        self.timestamp = datetime.now().isoformat()
        self.status = status.strip() if status else "00"
        self.source = source
    
    def to_dict(self):
        return {
            "recordId": self.record_id,
            "file": self.file,
            "operation": self.operation,
            "timestamp": self.timestamp,
            "status": self.status,
            "source": self.source
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
            "transactions": [
                {
                    "source": t.source if hasattr(t, 'source') else "COBOL",
                    "recordId": t.record_id if hasattr(t, 'record_id') else t.get('recordId'),
                    "operation": t.operation if hasattr(t, 'operation') else t.get('operation'),
                    "file": t.file if hasattr(t, 'file') else t.get('file')
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
                "total": len(blockchain_blocks)
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
                    }
                },
                "totalFiles": len(set(t.file for t in cobol_transactions)),
                "totalRecords": len(cobol_transactions)
            }
            self.wfile.write(json.dumps(schema_info).encode())
        else:
            self.wfile.write(json.dumps({"error": "Not found"}).encode())
    
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

def parse_json_transaction(line):
    """Parse a JSON transaction from log line"""
    try:
        # Remove any prefixes like "COBOL_DIRECT: " or "BLOCKCHAIN WRITE: "
        if ":" in line:
            json_part = line.split(":", 1)[1].strip()
        else:
            json_part = line.strip()
            
        # Try to parse as JSON
        data = json.loads(json_part)
        
        # Extract fields with defaults
        record_id = data.get('record_id', data.get('recordId', 'UNKNOWN'))
        file = data.get('file', 'UNKNOWN.DAT')
        operation = data.get('operation', 'UNKNOWN')
        status = data.get('status', '00')
        source = data.get('source', 'COBOL')
        
        return Transaction(record_id, file, operation, status, source)
    except:
        return None

def monitor_log_file(log_path):
    """Monitor a log file for new transactions"""
    global transaction_counter, block_counter
    
    # Keep track of file position
    file_pos = 0
    
    while True:
        try:
            if os.path.exists(log_path):
                with open(log_path, 'r') as f:
                    # Go to last position
                    f.seek(file_pos)
                    
                    # Read new lines
                    new_lines = f.readlines()
                    
                    for line in new_lines:
                        line = line.strip()
                        if not line:
                            continue
                            
                        # Try to parse as transaction
                        trans = parse_json_transaction(line)
                        if trans:
                            transaction_counter += 1
                            cobol_transactions.append(trans)
                            
                            # Create block
                            block_counter += 1
                            previous_hash = blockchain_blocks[-1].hash if blockchain_blocks else "0000000000000000"
                            block = Block(block_counter, [trans], previous_hash)
                            blockchain_blocks.append(block)
                            
                            print(f"CAPTURED: {trans.to_dict()}")
                    
                    # Update position
                    file_pos = f.tell()
                    
        except Exception as e:
            print(f"Error monitoring {log_path}: {e}")
            
        time.sleep(1)  # Check every second

def monitor_cobol():
    """Monitor COBOL files and create transactions"""
    global transaction_counter, block_counter
    
    print("COBOL-Blockchain adapter started")
    log_dir = "/app/logs"
    data_dir = "/app/data"
    pipes_dir = "/app/pipes"
    
    # Create directories
    for dir_path in [log_dir, data_dir, pipes_dir]:
        os.makedirs(dir_path, exist_ok=True)
    
    # Initialize with some test data
    initial_transactions = [
        Transaction("TEST001", "MASTER.DAT", "CREATE"),
        Transaction("TEST002", "MASTER.DAT", "UPDATE")
    ]
    
    for trans in initial_transactions:
        transaction_counter += 1
        cobol_transactions.append(trans)
        
        # Create block for each transaction
        block_counter += 1
        previous_hash = blockchain_blocks[-1].hash if blockchain_blocks else "0000000000000000"
        block = Block(block_counter, [trans], previous_hash)
        blockchain_blocks.append(block)
    
    # Start monitoring blockchain writes log
    log_monitor_thread = threading.Thread(
        target=monitor_log_file, 
        args=(f"{log_dir}/blockchain-writes.log",),
        daemon=True
    )
    log_monitor_thread.start()
    
    # Monitor for other sources
    while True:
        try:
            # Check for COBOL data files
            if os.path.exists(data_dir):
                for file in os.listdir(data_dir):
                    if file.endswith('.dat'):
                        # Simulate periodic file updates
                        if transaction_counter % 10 == 0:  # Less frequent
                            transaction_counter += 1
                            trans = Transaction(
                                f"FILE{transaction_counter:06d}",
                                file.upper(),
                                "DETECT"
                            )
                            cobol_transactions.append(trans)
                            
                            # Create blockchain block
                            block_counter += 1
                            previous_hash = blockchain_blocks[-1].hash if blockchain_blocks else "0000000000000000"
                            block = Block(block_counter, [trans], previous_hash)
                            blockchain_blocks.append(block)
                            
                            # Also log to file
                            with open(f"{log_dir}/blockchain-writes.log", "a") as f:
                                f.write(f"{json.dumps(trans.to_dict())}\n")
            
            # Check for pipe messages
            pipe_path = f"{pipes_dir}/blockchain.pipe"
            if os.path.exists(pipe_path) and os.path.getsize(pipe_path) > 0:
                try:
                    with open(pipe_path, 'r') as pipe:
                        content = pipe.read()
                        # Clear the pipe
                        open(pipe_path, 'w').close()
                        
                        # Process each line
                        for line in content.strip().split('\n'):
                            if line:
                                trans = parse_json_transaction(line)
                                if trans:
                                    transaction_counter += 1
                                    cobol_transactions.append(trans)
                                    
                                    # Create block
                                    block_counter += 1
                                    previous_hash = blockchain_blocks[-1].hash if blockchain_blocks else "0000000000000000"
                                    block = Block(block_counter, [trans], previous_hash)
                                    blockchain_blocks.append(block)
                                    
                                    print(f"PIPE: {trans.to_dict()}")
                except Exception as e:
                    print(f"Pipe error: {e}")
            
        except Exception as e:
            print(f"Monitor error: {e}")
        
        time.sleep(5)  # Check every 5 seconds

def main():
    # Start API server in a separate thread
    api_thread = threading.Thread(target=start_api_server, daemon=True)
    api_thread.start()
    
    # Start COBOL monitoring
    monitor_cobol()

if __name__ == "__main__":
    main()