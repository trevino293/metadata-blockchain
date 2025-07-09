# metadata-blockchain

<div align="center">

```
╔════════════════════════════════════════════════════════════════════════════════╗
║                                                                                ║
║  ███╗   ███╗███████╗████████╗ █████╗ ██████╗  █████╗ ████████╗ █████╗          ║
║  ████╗ ████║██╔════╝╚══██╔══╝██╔══██╗██╔══██╗██╔══██╗╚══██╔══╝██╔══██╗         ║
║  ██╔████╔██║█████╗     ██║   ███████║██║  ██║███████║   ██║   ███████║         ║
║  ██║╚██╔╝██║██╔══╝     ██║   ██╔══██║██║  ██║██╔══██║   ██║   ██╔══██║         ║
║  ██║ ╚═╝ ██║███████╗   ██║   ██║  ██║██████╔╝██║  ██║   ██║   ██║  ██║         ║
║  ╚═╝     ╚═╝╚══════╝   ╚═╝   ╚═╝  ╚═╝╚═════╝ ╚═╝  ╚═╝   ╚═╝   ╚═╝  ╚═╝         ║
║                                         |                                      ║
║  ██████╗ ██╗      ██████╗  ██████╗██╗  ██╗ ██████╗██╗  ██╗ █████╗ ██╗███╗   ██╗║
║  ██╔══██╗██║     ██╔═══██╗██╔════╝██║ ██╔╝██╔════╝██║  ██║██╔══██╗██║████╗  ██║║
║  ██████╔╝██║     ██║   ██║██║     █████╔╝ ██║     ███████║███████║██║██╔██╗ ██║║
║  ██╔══██╗██║     ██║   ██║██║     ██╔═██╗ ██║     ██╔══██║██╔══██║██║██║╚██╗██║║
║  ██████╔╝███████╗╚██████╔╝╚██████╗██║  ██╗╚██████╗██║  ██║██║  ██║██║██║ ╚████║║
║  ╚═════╝ ╚══════╝ ╚═════╝  ╚═════╝╚═╝  ╚═╝ ╚═════╝╚═╝  ╚═╝╚═╝  ╚═╝╚═╝╚═╝  ╚═══╝║
║                                                                                ║
║                     🔗 PRIVATE METADATA BLOCKCHAIN PLATFORM 🔗                ║
║                                                                                ║
╚════════════════════════════════════════════════════════════════════════════════╝
```

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![PowerShell](https://img.shields.io/badge/PowerShell-%235391FE.svg?style=flat&logo=powershell&logoColor=white)](https://docs.microsoft.com/en-us/powershell/)
[![COBOL](https://img.shields.io/badge/COBOL-Legacy--Ready-blue.svg)](https://www.gnu.org/software/gnucobol/)
[![GCP](https://img.shields.io/badge/Google%20Cloud-%234285F4.svg?style=flat&logo=google-cloud&logoColor=white)](https://cloud.google.com/)
[![Georgia Tech](https://img.shields.io/badge/Georgia%20Tech-CS6795-B3A369.svg?style=flat&logo=graduation-cap&logoColor=white)](https://omscs.gatech.edu/)
[![Term Project](https://img.shields.io/badge/Term%20Project-Summer%202025-003057.svg?style=flat&logo=calendar&logoColor=white)](https://omscs.gatech.edu/cs-6795-introduction-cognitive-science)

**A private metadata blockchain platform for capturing and persisting data operations across heterogeneous systems, featuring COBOL legacy system integration and Google Cloud services.**

</div>

---

## 🎯 Project Overview

This project demonstrates metadata persistence using blockchain technology across distributed data sources. The system captures metadata from legacy COBOL systems and cloud-based data stores, creating an immutable audit trail for data governance and compliance.

### Key Features

- **Legacy System Integration**: COBOL metadata capture with real-time blockchain persistence
- **Cloud Integration**: Google Cloud Pub/Sub and VM-based blockchain nodes
- **Simple Deployment**: No complex infrastructure - just PowerShell scripts
- **Web Dashboard**: Real-time monitoring and management interface
- **API Integration**: RESTful APIs for external system connectivity

## 🏗️ Architecture

```
┌─────────────────┐    ┌──────────────────┐    ┌─────────────────┐
│   COBOL Node    │    │  GCP Cloud Node  │    │ Blockchain API  │
│   (Local)       │◄──►│   (Pub/Sub)      │◄──►│   Dashboard     │
└─────────────────┘    └──────────────────┘    └─────────────────┘
         │                        │                       │
         └────────────────────────┼───────────────────────┘
                                  ▼
                        ┌──────────────────┐
                        │ Blockchain       │
                        │ VM Instance      │
                        └──────────────────┘
```

## 🚀 Quick Start

### Prerequisites

- **Windows PowerShell 5.1+** (or PowerShell 7+)
- **Google Cloud SDK** (gcloud CLI)
- **Docker & Docker Compose** (for local COBOL node)
- **Google Cloud Project** with billing enabled

### Local COBOL Node Setup

1. **Clone the repository**
   ```powershell
   git clone <repository-url>
   cd docker-cobol-blockchain
   ```

2. **Build and start the COBOL blockchain node**
   ```bash
   docker-compose up -d
   ```

3. **Verify the setup**
   ```bash
   # Check container status
   docker-compose ps
   
   # View logs
   docker-compose logs -f cobol-blockchain
   ```

4. **Run COBOL tests**
   ```powershell
   .\tests\test-cobol-blockchain.ps1
   ```

### GCP Cloud Node Setup

1. **Configure GCP**
   ```powershell
   gcloud auth login
   gcloud config set project YOUR_PROJECT_ID
   ```

2. **Deploy cloud infrastructure**
   ```powershell
   cd gcp
   .\simple-gcp-deploy.ps1 -ProjectId "YOUR_PROJECT_ID"
   ```

3. **Test the deployment**
   ```powershell
   .\test-simple.ps1 -ProjectId "YOUR_PROJECT_ID"
   ```

4. **Monitor the blockchain**
   ```powershell
   # Open the generated HTML file in your browser
   .\blockchain-monitor.html
   ```

## 📁 Project Structure

```
metadata-blockchain/
├── docker-cobol-blockchain/         # Local COBOL blockchain node
│   ├── adapters/                    # Blockchain integration adapters
│   │   ├── cobol-fabric-adapter.py  # Main COBOL to blockchain adapter
│   │   └── config/                  # Adapter configuration files
│   ├── cobol_programs/              # COBOL source code
│   │   ├── customer-crud.cob        # Customer data operations
│   │   ├── metadata-capture.cob     # Metadata capture program
│   │   └── copybooks/               # Shared COBOL copybooks
│   ├── data/                        # Sample COBOL data files
│   │   ├── master.dat               # Master data file
│   │   ├── customer.dat             # Customer records
│   │   └── transactions.dat         # Transaction log
│   ├── scripts/                     # Automation scripts
│   │   ├── run-system.ps1           # System startup script
│   │   └── controlled-test.sh       # Test execution script
│   ├── logs/                        # System logs
│   ├── dashboard.html               # Web monitoring interface
│   ├── cobol-admin.html            # Administrative interface
│   └── docker-compose.yml          # Container orchestration
│
├── gcp/                             # GCP cloud deployment (SIMPLE)
│   ├── simple-gcp-deploy.ps1       # Deploy GCP resources
│   ├── test-simple.ps1             # Test the cloud setup
│   ├── cleanup-simple.ps1          # Clean up GCP resources
│   ├── connection-info.txt          # Generated connection details
│   └── blockchain-monitor.html     # Generated web monitor
│
└── README.md                        # This file
```

## 🔧 Configuration

### Local COBOL Node Configuration

Edit `docker-cobol-blockchain/adapters/config/adapter_config.json`:

```json
{
  "cobol": {
    "data_directory": "/app/data",
    "pipe_directory": "/tmp",
    "encoding": "ASCII",
    "record_format": "FIXED"
  },
  "blockchain": {
    "batch_size": 10,
    "timeout": 30,
    "gcp_pubsub_topic": "metadata-events",
    "gcp_project_id": "your-project-id"
  },
  "monitoring": {
    "file_patterns": ["*.dat", "*.idx"],
    "poll_interval": 1,
    "buffer_size": 1024
  }
}
```

### GCP Configuration

The PowerShell scripts automatically configure:
- **Pub/Sub Topic**: `metadata-events`
- **VM Instance**: `blockchain-node` (e2-small)
- **Firewall**: Port 8080 for API access
- **Service Account**: Automatic permissions

## 🖥️ Web Interface

### Dashboard Access

1. **Local Dashboard**: Open `docker-cobol-blockchain/dashboard.html` in your browser
2. **Admin Interface**: Open `docker-cobol-blockchain/cobol-admin.html` for data management
3. **GCP Monitor**: Open `gcp/blockchain-monitor.html` for cloud monitoring
4. **API Endpoint**: http://YOUR_VM_IP:8080/

### API Endpoints

```powershell
# Get VM IP first
$vmIp = "YOUR_VM_IP"  # From deployment output

# System Status
Invoke-RestMethod -Uri "http://$vmIp:8080/status"

# Blockchain Blocks
Invoke-RestMethod -Uri "http://$vmIp:8080/blocks"

# Pending Transactions
Invoke-RestMethod -Uri "http://$vmIp:8080/transactions"
```

## 🔍 Monitoring & Operations

### Real-time Monitoring

```powershell
# Monitor GCP VM logs
gcloud compute ssh blockchain-node --zone=us-central1-a --command='sudo journalctl -u blockchain -f' --project=YOUR_PROJECT_ID

# Monitor local COBOL node
docker logs -f cobol-metadata-node

# Check system status
Invoke-RestMethod -Uri "http://$vmIp:8080/status"
```

### COBOL Operations

```bash
# Run COBOL metadata capture
docker exec cobol-metadata-node /app/metadata-capture

# Execute CRUD operations
docker exec cobol-metadata-node bash -c "export COB_LIBRARY_PATH=/app && /app/test-crud"

# View COBOL data files
docker exec cobol-metadata-node ls -la /app/data/
```

### Cloud Operations

```powershell
# Publish test message to Pub/Sub
gcloud pubsub topics publish metadata-events --message='{"operation":"TEST","entity":"Manual","id":"TEST_001"}' --project=YOUR_PROJECT_ID

# Check blockchain response
Start-Sleep -Seconds 10
Invoke-RestMethod -Uri "http://$vmIp:8080/blocks"

# Monitor Pub/Sub subscription
gcloud pubsub subscriptions describe blockchain-sub --project=YOUR_PROJECT_ID
```

## 🧪 Testing

### Automated Testing

```powershell
# Test local COBOL node
cd docker-cobol-blockchain
.\tests\test-cobol-blockchain.ps1

# Test GCP cloud node
cd gcp
.\test-simple.ps1 -ProjectId "YOUR_PROJECT_ID"
```

### Manual Testing

```powershell
# Create test data locally
echo "TEST999   Manual Test Record                   $(Get-Date -Format 'yyyy-MM-dd')" | docker exec -i cobol-metadata-node tee -a /app/data/test.dat

# Publish to cloud
gcloud pubsub topics publish metadata-events --message='{"operation":"CREATE","entity":"TestEntity","id":"MANUAL_001","source":"PowerShell"}' --project=YOUR_PROJECT_ID

# Verify blockchain creation
$vmIp = "YOUR_VM_IP"
Invoke-RestMethod -Uri "http://$vmIp:8080/blocks" | ConvertTo-Json -Depth 4
```

## 📊 Sample Data

The system includes comprehensive sample data:

### Local COBOL Data
- **MASTER.DAT**: 7 master records
- **CUSTOMER.DAT**: 5 customer profiles
- **TRANSACTION.DAT**: 5 transaction records
- **ACCOUNTS.DAT**: 5 account records
- **PRODUCTS.IDX**: 5 product entries

### Cloud Test Data
```json
{
  "operation": "CREATE",
  "entity_type": "Customer",
  "entity_id": "CUST_001",
  "source": "CloudTest",
  "timestamp": "2025-01-21T15:30:00Z",
  "metadata": {
    "table": "customers",
    "user_id": "admin",
    "session_id": "sess_12345"
  }
}
```

## 🔒 Security Considerations

- Private blockchain network with simple authentication
- GCP firewall rules restrict access to port 8080
- Service account with minimal required permissions
- Local Docker network isolation
- No sensitive data in logs or configuration files

## 🚨 Troubleshooting

### Common Issues

1. **GCP VM not responding**
   ```powershell
   # Check VM status
   gcloud compute instances list --project=YOUR_PROJECT_ID
   
   # Check VM logs
   gcloud compute ssh blockchain-node --zone=us-central1-a --command='sudo journalctl -u blockchain -n 20' --project=YOUR_PROJECT_ID
   ```

2. **Local COBOL container issues**
   ```bash
   # Check container status
   docker-compose ps
   
   # Restart services
   docker-compose restart
   ```

3. **API not responding**
   ```powershell
   # Test from VM itself
   gcloud compute ssh blockchain-node --zone=us-central1-a --command='curl localhost:8080/status' --project=YOUR_PROJECT_ID
   
   # Check firewall
   gcloud compute firewall-rules list --filter="name:allow-blockchain" --project=YOUR_PROJECT_ID
   ```

4. **Pub/Sub messages not processing**
   ```powershell
   # Check subscription backlog
   gcloud pubsub subscriptions describe blockchain-sub --project=YOUR_PROJECT_ID
   
   # Test direct publish
   gcloud pubsub topics publish metadata-events --message='{"test":"debug"}' --project=YOUR_PROJECT_ID
   ```

## 💰 Cost Optimization

### Development Environment
- **VM**: e2-small (~$15/month, can use free tier credits)
- **Pub/Sub**: $0.40 per million messages (very low for testing)
- **Egress**: Minimal for API calls
- **Total**: ~$15-20/month for development

### Cost Saving Tips
```powershell
# Stop VM when not needed
gcloud compute instances stop blockchain-node --zone=us-central1-a --project=YOUR_PROJECT_ID

# Start when needed
gcloud compute instances start blockchain-node --zone=us-central1-a --project=YOUR_PROJECT_ID

# Use preemptible instances for testing (add --preemptible flag)
```

## 🤝 Contributing

1. Fork the repository
2. Create a feature branch
3. Test with both local and cloud nodes
4. Submit a pull request with detailed description

## 📜 License

MIT License - see [LICENSE](LICENSE) file for details.

## 📞 Support

- **Documentation**: See inline code comments and this README
- **Issues**: GitHub Issues tracker
- **Development**: CS6795 Term Project (Summer 2025)

## 🎓 Academic Context

This project fulfills the CS6795 Cognitive Science term project requirements at Georgia Institute of Technology, focusing on the computational Model/Tool track. The research addresses private metadata blockchain implementation across heterogeneous data systems.

### Research Questions Addressed

1. **RQ1**: What metadata can be persisted within a blockchain across heterogeneous stores?
2. **RQ2**: What system architecture ensures reliable metadata persistence during schema changes?
3. **RQ3**: What are the privacy, legal, and compliance benefits of private metadata blockchain?

## 🔧 Advanced Configuration

### Connecting Local and Cloud Nodes

To create a hybrid local-cloud blockchain network:

1. **Configure local node to publish to GCP Pub/Sub**:
   ```json
   // In docker-cobol-blockchain/adapters/config/adapter_config.json
   {
     "gcp": {
       "project_id": "YOUR_PROJECT_ID",
       "pubsub_topic": "metadata-events",
       "credentials_path": "/app/config/gcp-credentials.json"
     }
   }
   ```

2. **Set up cross-network communication**:
   ```powershell
   # Allow local IP to access GCP VM
   $localIp = (Invoke-WebRequest -Uri "https://ipinfo.io/ip").Content.Trim()
   gcloud compute firewall-rules update allow-blockchain --source-ranges="0.0.0.0/0,$localIp/32" --project=YOUR_PROJECT_ID
   ```

### Production Deployment

For production use:

1. **Use managed instance groups**
2. **Add load balancing**
3. **Implement proper authentication**
4. **Set up monitoring and alerting**
5. **Use Cloud SQL or Spanner for persistence**

---

**🔗 Quick Links**
- [Local Dashboard](./docker-cobol-blockchain/dashboard.html) - COBOL node monitoring
- [Cloud Monitor](./gcp/blockchain-monitor.html) - GCP node monitoring  
- [Admin Interface](./docker-cobol-blockchain/cobol-admin.html) - Data management
- [COBOL Programs](./docker-cobol-blockchain/cobol_programs/) - Source code
- [GCP Scripts](./gcp/) - Cloud deployment scripts