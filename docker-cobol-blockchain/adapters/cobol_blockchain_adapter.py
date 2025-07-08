import os
import json
import select
from datetime import datetime
from fabric_sdk_py import FabricClient

class COBOLMetadataAdapter:
    def __init__(self):
        self.pipe_path = "/tmp/cobol-triggers.pipe"
        self.ensure_pipe()
        self.fabric_client = FabricClient()
        
    def ensure_pipe(self):
        """Create named pipe if it doesn't exist"""
        if not os.path.exists(self.pipe_path):
            os.mkfifo(self.pipe_path)
    
    def monitor_cobol_triggers(self):
        """Monitor COBOL file operations via named pipe"""
        with open(self.pipe_path, 'r') as pipe:
            while True:
                # Non-blocking read
                readable, _, _ = select.select([pipe], [], [], 0.1)
                
                if readable:
                    line = pipe.readline().strip()
                    if line and line.endswith('@'):  # COBOL delimiter
                        self.process_cobol_metadata(line[:-1])
    
    def process_cobol_metadata(self, metadata_str):
        """Process COBOL metadata and submit to blockchain"""
        try:
            metadata = json.loads(metadata_str)
            
            # Enrich metadata
            blockchain_entry = {
                'source': 'COBOL-LEGACY',
                'timestamp': metadata.get('timestamp', datetime.now().isoformat()),
                'operation': metadata.get('operation'),
                'file': metadata.get('file'),
                'record_id': metadata.get('record_id'),
                'file_status': metadata.get('status'),
                'system_type': 'GnuCOBOL',
                'data_format': 'INDEXED-SEQUENTIAL'
            }
            
            # Submit to Hyperledger Fabric
            self.submit_to_blockchain(blockchain_entry)
            
        except json.JSONDecodeError as e:
            print(f"Error parsing COBOL metadata: {e}")
    
    def submit_to_blockchain(self, metadata):
        """Submit metadata to Hyperledger Fabric"""
        response = self.fabric_client.chaincode_invoke(
            channel_name='metadata-channel',
            chaincode_name='metadata-cc',
            function='createMetadataEntry',
            args=[json.dumps(metadata)]
        )
        print(f"Blockchain transaction: {response['transaction_id']}")