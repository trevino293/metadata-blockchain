# Save as: deploy-adapter.ps1

Write-Host "=== Deploying Async Adapter ===" -ForegroundColor Green

# Create async-crud-adapter.py
@'
#!/usr/bin/env python3
import json
import asyncio
import hashlib
import datetime
import os
import signal
import sys

class AsyncCOBOLBlockchainAdapter:
    def __init__(self):
        self.pipe_path = "/app/pipes/crud-events"
        self.tx_queue = asyncio.Queue(maxsize=1000)
        self.processed_events = set()
        self.running = True
        
    async def setup_pipe(self):
        os.makedirs(os.path.dirname(self.pipe_path), exist_ok=True)
        if os.path.exists(self.pipe_path):
            os.remove(self.pipe_path)
        os.mkfifo(self.pipe_path)
        print(f"[SETUP] Created named pipe: {self.pipe_path}")
    
    async def monitor_pipe(self):
        print("[MONITOR] Starting pipe monitor...")
        while self.running:
            try:
                with open(self.pipe_path, 'r') as pipe:
                    for line in pipe:
                        if line.strip():
                            await self.process_event(line.strip())
            except Exception as e:
                print(f"[ERROR] Pipe monitor: {e}")
                await asyncio.sleep(1)
    
    async def process_event(self, event_line):
        try:
            event_data = json.loads(event_line)
            event_hash = hashlib.sha256(event_line.encode()).hexdigest()
            
            if event_hash not in self.processed_events:
                self.processed_events.add(event_hash)
                
                tx_id = hashlib.sha256(
                    f"{datetime.datetime.now().isoformat()}{event_line}".encode()
                ).hexdigest()[:16]
                
                tx = {
                    "tx_id": tx_id,
                    "timestamp": datetime.datetime.now().isoformat(),
                    "function": "RecordCRUDOperation",
                    "args": [
                        tx_id,
                        event_data.get("operation"),
                        event_data.get("entity"),
                        event_data.get("id"),
                        event_data.get("timestamp"),
                        json.dumps(event_data),
                        "CUSTOMER-CRUD",
                        "ASYNC"
                    ],
                    "status": "submitted",
                    "peer": "peer0.org1.example.com:7051"
                }
                
                with open("/app/logs/fabric-transactions.log", "a") as f:
                    f.write(json.dumps(tx) + "\n")
                
                print(f"[EVENT] {event_data['operation']} {event_data['id']} -> TX: {tx_id}")
                
        except Exception as e:
            print(f"[ERROR] Processing event: {e}")
    
    async def run(self):
        await self.setup_pipe()
        await self.monitor_pipe()

def main():
    adapter = AsyncCOBOLBlockchainAdapter()
    asyncio.run(adapter.run())

if __name__ == "__main__":
    main()
'@ | Out-File -FilePath "async-crud-adapter.py" -Encoding UTF8

docker cp async-crud-adapter.py cobol-metadata-node:/app/adapters/
docker exec cobol-metadata-node chmod +x /app/adapters/async-crud-adapter.py

Remove-Item "async-crud-adapter.py"
Write-Host "✅ Adapter deployed" -ForegroundColor Green