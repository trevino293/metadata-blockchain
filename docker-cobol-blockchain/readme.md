# Docker COBOL Blockchain Node

## 🎯 Overview

The Docker COBOL Blockchain Node provides a containerized environment for capturing metadata from legacy COBOL systems and integrating with blockchain networks. This component simulates a legacy mainframe environment while providing modern blockchain connectivity.

## 🏗️ Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                Docker COBOL Blockchain Node                 │
├─────────────────────────────────────────────────────────────┤
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────────────┐  │
│  │   COBOL     │  │  Adapters   │  │   Web Interfaces    │  │
│  │  Programs   │◄─│             │◄─│                     │  │
│  │             │  │ Fabric      │  │ dashboard.html      │  │
│  │ - Metadata  │  │ GCP PubSub  │  │ cobol-admin.html    │  │
│  │ - CRUD      │  │             │  │                     │  │
│  │ - Capture   │  │             │  │                     │  │
│  └─────────────┘  └─────────────┘  └─────────────────────┘  │
│         │                │                      │           │
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────────────┐  │
│  │    Data     │  │    Logs     │  │       Scripts       │  │
│  │ Files       │  │             │  │                     │  │
│  │             │  │ adapter.log │  │ setup-fabric.ps1    │  │
│  │ master.dat  │  │ system.log  │  │ test-system.sh      │  │
│  │ customer.dat│  │             │  │ create-pipes.sh     │  │
│  └─────────────┘  └─────────────┘  └─────────────────────┘  │
└─────────────────────────────────────────────────────────────┘
```

## 🚀 Quick Start

### Prerequisites

- **Docker** (version 20.0+)
- **Docker Compose** (version 1.28+)
- **PowerShell** (for Windows scripts)
- **Bash** (for Linux/Unix scripts)

### 1. Basic Setup

```bash
# Clone and navigate to directory
cd docker-cobol-blockchain

# Build and start services
docker-compose up -d

# Verify containers are running
docker-compose ps
```

### 2. Initialize the System

```bash
# Initialize blockchain components (if needed)
./scripts/setup-fabric-minimal.ps1  # Windows
# or
./scripts/setup-fabric-minimal.sh   # Linux

# Create data directories and pipes
docker exec cobol-metadata-node ./scripts/create-pipes.sh
```

### 3. Test the Setup

```bash
# Run comprehensive tests
./tests/test-cobol-blockchain.ps1    # Windows
# or
./tests/test-cobol-blockchain.sh     # Linux

# Check logs
docker-compose logs -f cobol-blockchain
```

## 📁 Directory Structure

```
docker-cobol-blockchain/
├── adapters/                        # Blockchain integration
│   ├── cobol-fabric-adapter.py      # Main Hyperledger Fabric adapter
│   ├── gcp-pubsub-adapter.py       # Google Cloud Pub/Sub adapter
│   └── config/
│       └── adapter_config.json     # Adapter configuration
├── cobol_programs/                  # COBOL source code
│   ├── metadata-capture.cob        # Metadata capture program
│   ├── customer-crud.cob          # Customer CRUD operations
│   ├── test-crud.cob              # Test program
│   └── copybooks/                 # Shared COBOL definitions
│       ├── customer-record.cpy    # Customer record structure
│       └── metadata-record.cpy    # Metadata record structure
├── data/                           # Sample data files
│   ├── master.dat                 # Master data (7 records)
│   ├── customer.dat              # Customer data (5 records)
│   ├── transaction.dat           # Transaction log (5 records)
│   ├── accounts.dat              # Account data (5 records)
│   └── products.idx              # Product index (5 entries)
├── scripts/                       # Automation scripts
│   ├── setup-fabric-minimal.ps1  # Fabric setup (Windows)
│   ├── setup-fabric-minimal.sh   # Fabric setup (Linux)
│   ├── create-pipes.sh           # Named pipe creation
│   ├── run-system.ps1            # System startup
│   └── controlled-test.sh        # Test execution
├── tests/                         # Test scripts
│   ├── test-cobol-blockchain.ps1  # Main test suite (Windows)
│   ├── test-cobol-blockchain.sh   # Main test suite (Linux)
│   └── incremental-test-cobol-blockchain.ps1  # Incremental tests
├── logs/                          # System logs
│   ├── adapter.log               # Adapter activity
│   ├── system.log               # System events
│   ├── fabric-transactions.log  # Blockchain transactions
│   └── blockchain-writes.log    # Blockchain write operations
├── dashboard.html                # Web monitoring interface
├── cobol-admin.html             # Administrative interface
├── docker-compose.yml          # Container orchestration
├── Dockerfile                  # Container definition
├── start-services.sh           # Container startup script
└── README.md                   # This file
```

## 🔧 Configuration

### Docker Compose Configuration

The `docker-compose.yml` includes:

```yaml
version: '3.8'
services:
  hyperledger-peer:
    image: hyperledger/fabric-peer:2.5
    container_name: peer0.org1.example.com
    # Hyperledger Fabric peer configuration
    
  cobol-blockchain:
    build: .
    container_name: cobol-metadata-node
    volumes:
      - ./data:/app/data
      - ./logs:/app/logs
    networks:
      - blockchain-net
    ports:
      - "8080:8080"  # API/Dashboard port
```

### Adapter Configuration

Edit `adapters/config/adapter_config.json`:

```json
{
  "cobol": {
    "data_directory": "/app/data",
    "pipe_directory": "/tmp",
    "encoding": "ASCII",
    "record_format": "FIXED"
  },
  "blockchain": {
    "channel": "metadata-channel",
    "chaincode": "metadata-cc",
    "batch_size": 10,
    "timeout": 30
  },
  "monitoring": {
    "file_patterns": ["*.dat", "*.idx"],
    "poll_interval": 1,
    "buffer_size": 1024
  },
  "gcp": {
    "project_id": "your-project-id",
    "pubsub_topic": "metadata-events",
    "credentials_path": "/app/config/gcp-credentials.json"
  }
}
```

## 🖥️ Web Interfaces

### 1. Dashboard (dashboard.html)

Real-time monitoring interface accessible at `http://localhost:8080` or by opening the HTML file:

**Features:**
- System status monitoring
- Transaction history
- Block creation tracking
- Error reporting
- Performance metrics

**Usage:**
```bash
# Open in browser
open dashboard.html  # macOS
# or access via container port
curl http://localhost:8080/status
```

### 2. Admin Interface (cobol-admin.html)

Administrative control panel for COBOL operations:

**Features:**
- Data file management
- Manual metadata capture
- COBOL program execution
- Log file viewing
- Configuration management

**Usage:**
```bash
# Execute COBOL programs
docker exec cobol-metadata-node ./customer-crud
docker exec cobol-metadata-node ./metadata-capture
```

## 🧪 Testing

### Automated Test Suite

Run the comprehensive test suite:

```powershell
# Windows PowerShell
.\tests\test-cobol-blockchain.ps1

# Options:
.\tests\test-cobol-blockchain.ps1 -Verbose
.\tests\test-cobol-blockchain.ps1 -SkipBuild
.\tests\test-cobol-blockchain.ps1 -GcpProjectId "your-project"
```

```bash
# Linux/Unix
./tests/test-cobol-blockchain.sh

# With options
./tests/test-cobol-blockchain.sh --verbose
./tests/test-cobol-blockchain.sh --skip-build
```

### Manual Testing

```bash
# 1. Test COBOL data capture
echo "TEST999   Manual Test Record                   $(date '+%Y-%m-%d')" | docker exec -i cobol-metadata-node tee -a /app/data/test.dat

# 2. Trigger metadata capture
docker exec cobol-metadata-node ./metadata-capture

# 3. Check blockchain logs
docker exec cobol-metadata-node tail -f /app/logs/fabric-transactions.log

# 4. Verify data in blockchain
docker exec cobol-metadata-node cat /app/logs/blockchain-writes.log
```

### Integration Testing

```bash
# Test with GCP integration (requires project setup)
export GCP_PROJECT_ID="your-project-id"

# Test Pub/Sub connectivity
docker exec -e GOOGLE_CLOUD_PROJECT=$GCP_PROJECT_ID cobol-metadata-node python3 /app/adapters/gcp-pubsub-adapter.py

# Test Fabric integration
docker exec cobol-metadata-node python3 /app/adapters/cobol-fabric-adapter.py
```

## 📊 Monitoring & Logging

### Log Files

1. **System Logs**
   ```bash
   # View adapter activity
   docker exec cobol-metadata-node tail -f /app/logs/adapter.log
   
   # View system events
   docker exec cobol-metadata-node tail -f /app/logs/system.log
   ```

2. **Blockchain Logs**
   ```bash
   # Fabric transaction log
   docker exec cobol-metadata-node tail -f /app/logs/fabric-transactions.log
   
   # Blockchain write operations
   docker exec cobol-metadata-node tail -f /app/logs/blockchain-writes.log
   ```

3. **Docker Logs**
   ```bash
   # Container logs
   docker-compose logs -f cobol-blockchain
   
   # Specific container
   docker logs -f cobol-metadata-node
   ```

### Performance Monitoring

```bash
# Container resource usage
docker stats cobol-metadata-node

# Detailed container inspection
docker exec cobol-metadata-node ps aux
docker exec cobol-metadata-node df -h
docker exec cobol-metadata-node free -m
```

### Health Checks

```bash
# Check container health
docker exec cobol-metadata-node ./scripts/health-check.sh

# Verify blockchain connectivity
docker exec cobol-metadata-node python3 -c "
import sys
sys.path.append('/app/adapters')
from cobol_fabric_adapter import test_connection
test_connection()
"

# Test COBOL programs
docker exec cobol-metadata-node bash -c "
cd /app && 
export COB_LIBRARY_PATH=/app && 
./test-crud
"
```

## 🚨 Troubleshooting

### Common Issues

1. **Container Won't Start**
   ```bash
   # Check Docker status
   docker system info
   
   # Rebuild containers
   docker-compose down
   docker-compose build --no-cache
   docker-compose up -d
   
   # Check container logs
   docker-compose logs cobol-blockchain
   ```

2. **COBOL Programs Not Found**
   ```bash
   # Verify COBOL installation
   docker exec cobol-metadata-node which cobc
   docker exec cobol-metadata-node cobc --version
   
   # Check program compilation
   docker exec cobol-metadata-node ls -la /app/*.cob
   docker exec cobol-metadata-node ls -la /app/customer-crud
   ```

3. **Blockchain Connection Issues**
   ```bash
   # Check Hyperledger Fabric peer
   docker exec peer0.org1.example.com peer version
   
   # Test peer connectivity
   docker exec cobol-metadata-node ping peer0.org1.example.com
   
   # Check certificates
   docker exec cobol-metadata-node ls -la /etc/hyperledger/fabric/msp/
   ```

4. **Data File Issues**
   ```bash
   # Check data directory permissions
   docker exec cobol-metadata-node ls -la /app/data/
   
   # Verify file formats
   docker exec cobol-metadata-node file /app/data/*.dat
   
   # Test file access
   docker exec cobol-metadata-node cat /app/data/master.dat
   ```

5. **Port Conflicts**
   ```bash
   # Check port usage
   netstat -an | grep 8080
   
   # Use different port
   docker-compose down
   # Edit docker-compose.yml: "8081:8080"
   docker-compose up -d
   ```

### Debug Mode

Enable verbose debugging:

```bash
# Set debug environment
docker-compose down
docker-compose up -d --verbose

# Enable adapter debugging
docker exec cobol-metadata-node python3 /app/adapters/cobol-fabric-adapter.py --debug

# Enable COBOL debugging
docker exec cobol-metadata-node bash -c "
export COB_SET_DEBUG=1
export COB_SET_TRACE=1
./metadata-capture
"
```

### Performance Issues

```bash
# Check system resources
docker exec cobol-metadata-node top
docker exec cobol-metadata-node iostat

# Optimize container resources
# Edit docker-compose.yml:
# services:
#   cobol-blockchain:
#     deploy:
#       resources:
#         limits:
#           memory: 1G
#           cpus: '0.5'
```

## 🔧 Advanced Configuration

### Custom COBOL Programs

1. **Add new COBOL program**:
   ```bash
   # Create new program
   cat > cobol_programs/new-program.cob << 'EOF'
   IDENTIFICATION DIVISION.
   PROGRAM-ID. NEW-PROGRAM.
   
   DATA DIVISION.
   WORKING-STORAGE SECTION.
   01 WS-MESSAGE PIC X(50) VALUE 'Hello from new program'.
   
   PROCEDURE DIVISION.
   DISPLAY WS-MESSAGE.
   STOP RUN.
   EOF
   
   # Compile in container
   docker exec cobol-metadata-node cobc -x -o /app/new-program /app/cobol_programs/new-program.cob
   
   # Test execution
   docker exec cobol-metadata-node ./new-program
   ```

2. **Integrate with blockchain**:
   ```python
   # Add to adapters/cobol-fabric-adapter.py
   def handle_new_program_metadata(metadata):
       # Process metadata from new program
       pass
   ```

### External Integration

1. **Connect to external Fabric network**:
   ```json
   // adapters/config/adapter_config.json
   {
     "fabric": {
       "peer_endpoint": "external-peer.example.com:7051",
       "channel": "external-channel",
       "msp_id": "ExternalOrgMSP"
     }
   }
   ```

2. **Add database connectivity**:
   ```bash
   # Install database drivers
   docker exec cobol-metadata-node pip3 install psycopg2-binary mysql-connector-python
   ```

### Production Deployment

1. **Security hardening**:
   ```yaml
   # docker-compose.yml
   services:
     cobol-blockchain:
       security_opt:
         - no-new-privileges:true
       read_only: true
       tmpfs:
         - /tmp
       user: "1000:1000"
   ```

2. **Persistent storage**:
   ```yaml
   # docker-compose.yml
   volumes:
     cobol-data:
       driver: local
     cobol-logs:
       driver: local
   ```

## 📈 Performance Optimization

### Container Optimization

```bash
# Use multi-stage builds (edit Dockerfile)
FROM gnucobol AS builder
# ... build steps ...

FROM ubuntu:20.04
COPY --from=builder /app/binaries /app/
```

### Resource Tuning

```yaml
# docker-compose.yml
services:
  cobol-blockchain:
    deploy:
      resources:
        limits:
          memory: 2G
          cpus: '1.0'
        reservations:
          memory: 512M
          cpus: '0.25'
```

## 🔗 Integration Examples

### With External Systems

```python
# Custom adapter example
class CustomSystemAdapter:
    def __init__(self):
        self.config = load_config()
    
    def capture_metadata(self, event):
        # Process event from external system
        metadata = self.extract_metadata(event)
        self.send_to_blockchain(metadata)
```

### With Cloud Services

```bash
# GCP Integration
export GOOGLE_APPLICATION_CREDENTIALS="/app/config/gcp-credentials.json"
docker exec cobol-metadata-node python3 /app/adapters/gcp-pubsub-adapter.py
```

## 📚 References

- [GNU COBOL Documentation](https://gnucobol.sourceforge.io/)
- [Hyperledger Fabric Documentation](https://hyperledger-fabric.readthedocs.io/)
- [Docker Compose Reference](https://docs.docker.com/compose/)
- [Main Project README](../README.md)

---

**Next Steps:**
1. Review [GCP README](../gcp/README.md) for cloud deployment
2. Check [Test Documentation](./tests/README.md) for testing details
3. See [Main Project README](../README.md) for complete system overview