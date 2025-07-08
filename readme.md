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

[![Build Status](https://img.shields.io/badge/build-passing-brightgreen.svg)](https://github.com/your-org/metadata-blockchain)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Docker](https://img.shields.io/badge/docker-%230db7ed.svg?style=flat&logo=docker&logoColor=white)](https://docker.com)
[![Terraform](https://img.shields.io/badge/terraform-%235835CC.svg?style=flat&logo=terraform&logoColor=white)](https://terraform.io)
[![COBOL](https://img.shields.io/badge/COBOL-Legacy--Ready-blue.svg)](https://www.gnu.org/software/gnucobol/)
[![Hyperledger](https://img.shields.io/badge/Hyperledger-Fabric-orange.svg)](https://hyperledger-fabric.readthedocs.io/)

**A private metadata blockchain computational tool for capturing and persisting data operations across heterogeneous systems, featuring COBOL legacy system integration and modern cloud data sources.**

</div>

---

## 🎯 Project Overview

This project demonstrates a novel approach to metadata persistence using blockchain technology across distributed data sources. The system captures metadata from legacy COBOL systems and cloud-based data stores, creating an immutable audit trail for data governance and compliance.

### Key Features

- **Legacy System Integration**: COBOL metadata capture with real-time blockchain persistence
- **Multi-Node Architecture**: Peer nodes across local and cloud environments
- **Web Dashboard**: Real-time monitoring and management interface
- **API Integration**: RESTful APIs for external system connectivity
- **Hyperledger Fabric**: Enterprise-grade blockchain implementation

## 🏗️ Architecture

```
┌─────────────────┐    ┌──────────────────┐    ┌─────────────────┐
│   COBOL Node    │    │  GCP Cloud Node  │    │ Blockchain API  │
│   (Local)       │◄──►│   (Terraform)    │◄──►│   Dashboard     │
└─────────────────┘    └──────────────────┘    └─────────────────┘
         │                        │                       │
         └────────────────────────┼───────────────────────┘
                                  ▼
                        ┌──────────────────┐
                        │ Hyperledger      │
                        │ Fabric Network   │
                        └──────────────────┘
```

## 🚀 Quick Start

### Prerequisites

- Docker & Docker Compose
- Node.js 18+ (for dashboard)
- Terraform (for GCP deployment)
- Google Cloud SDK (for GCP components)
- PowerShell (for setup scripts on Windows)

### Local COBOL Node Setup

1. **Clone the repository**
   ```bash
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

4. **Run initial COBOL tests**
   ```powershell
   # Windows PowerShell
   .\tests\test-cobol-blockchain.ps1
   
   # Or manually
   docker exec cobol-metadata-node /app/scripts/controlled-test.sh
   ```

### GCP Cloud Node Setup

1. **Configure GCP credentials**
   ```bash
   gcloud auth login
   gcloud config set project YOUR_PROJECT_ID
   ```

2. **Initialize Terraform**
   ```bash
   cd terraform/gcp-node
   terraform init
   ```

3. **Deploy cloud infrastructure**
   ```bash
   # Review the plan
   terraform plan -var="project_id=YOUR_PROJECT_ID"
   
   # Apply configuration
   terraform apply -var="project_id=YOUR_PROJECT_ID"
   ```

4. **Configure network connectivity**
   ```bash
   # Get cloud node IP
   terraform output cloud_node_ip
   
   # Update local node configuration
   # Edit docker-cobol-blockchain/adapters/config/adapter_config.json
   ```

## 📁 Project Structure

```
docker-cobol-blockchain/
├── adapters/                    # Blockchain integration adapters
│   ├── cobol-fabric-adapter.py  # Main COBOL to blockchain adapter
│   └── config/                  # Adapter configuration files
├── cobol_programs/              # COBOL source code
│   ├── customer-crud.cob        # Customer data operations
│   ├── metadata-capture.cob     # Metadata capture program
│   └── copybooks/               # Shared COBOL copybooks
├── data/                        # Sample COBOL data files
│   ├── master.dat               # Master data file
│   ├── customer.dat             # Customer records
│   └── transactions.dat         # Transaction log
├── scripts/                     # Automation scripts
│   ├── run-system.ps1           # System startup script
│   └── controlled-test.sh       # Test execution script
├── logs/                        # System logs
├── blockchain/                  # Hyperledger Fabric configuration
├── dashboard.html               # Web monitoring interface
├── cobol-admin.html            # Administrative interface
└── docker-compose.yml          # Container orchestration

terraform/
├── gcp-node/                    # GCP cloud node infrastructure
│   ├── main.tf                  # Main Terraform configuration
│   ├── variables.tf             # Input variables
│   ├── outputs.tf               # Output values
│   └── modules/                 # Reusable Terraform modules
│       ├── compute/             # Compute Engine instances
│       ├── storage/             # Cloud Storage and databases
│       └── networking/          # VPC and firewall rules
└── shared/                      # Shared Terraform resources
    └── blockchain-network/      # Cross-cloud blockchain network
```

## 🔧 Configuration

### COBOL Node Configuration

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
    "channel": "metadata-channel",
    "chaincode": "metadata-cc",
    "batch_size": 10,
    "timeout": 30,
    "peers": [
      "peer0.org1.example.com:7051",
      "gcp-peer.org2.example.com:7051"
    ]
  },
  "monitoring": {
    "file_patterns": ["*.dat", "*.idx"],
    "poll_interval": 1,
    "buffer_size": 1024
  }
}
```

### GCP Node Configuration

Configure `terraform/gcp-node/terraform.tfvars`:

```hcl
project_id = "your-gcp-project-id"
region     = "us-central1"
zone       = "us-central1-a"

# VM Configuration
vm_machine_type = "e2-standard-2"
vm_disk_size    = 50

# Network Configuration
network_name    = "blockchain-network"
subnet_name     = "blockchain-subnet"
subnet_cidr     = "10.0.1.0/24"

# Blockchain Configuration
blockchain_channel = "metadata-channel"
org_name          = "Org2MSP"
peer_name         = "gcp-peer"

# Firewall Rules
allowed_ports = [
  "7051",  # Peer port
  "7053",  # Peer event port
  "8080"   # API port
]

# Tags for resource management
common_tags = {
  environment = "development"
  project     = "cobol-blockchain"
  owner       = "blockchain-team"
}
```

## 🖥️ Web Interface

### Dashboard Access

1. **Local Dashboard**: Open `dashboard.html` in your browser
2. **Admin Interface**: Open `cobol-admin.html` for data management
3. **API Endpoint**: http://localhost:8080/api/

### API Endpoints

```bash
# System Status
GET http://localhost:8080/api/status

# Transaction History
GET http://localhost:8080/api/transactions

# Blockchain Blocks
GET http://localhost:8080/api/blocks

# System Statistics
GET http://localhost:8080/api/stats

# Schema Information
GET http://localhost:8080/api/schema

# Submit Manual Transaction
POST http://localhost:8080/api/transaction
Content-Type: application/json
{
  "recordId": "MANUAL001",
  "file": "MANUAL.DAT",
  "operation": "CREATE",
  "data": {"description": "Manual test transaction"}
}
```

## 🔍 Monitoring & Operations

### Real-time Monitoring

```bash
# Monitor adapter logs
docker logs -f cobol-metadata-node

# View blockchain transactions
docker exec cobol-metadata-node tail -f /app/logs/fabric-transactions.log

# System status check
docker exec cobol-metadata-node /app/scripts/status.sh
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

### Blockchain Operations

```bash
# Query all metadata
docker exec cobol-metadata-node python3 /app/scripts/fabric-query.py all

# Query by file
docker exec cobol-metadata-node python3 /app/scripts/fabric-query.py file MASTER.DAT

# Manual adapter run
docker exec cobol-metadata-node python3 /app/adapters/manual-adapter.py
```

## 🧪 Testing

### Automated Testing

```powershell
# Comprehensive system test
.\tests\test-cobol-blockchain.ps1

# Quick integration test
.\tests\debug-ui.ps1
```

### Manual Testing

```bash
# Create test data
echo "TEST999   Manual Test Record                   $(date +%Y-%m-%d)" | \
  docker exec -i cobol-metadata-node tee -a /app/data/test.dat

# Verify transaction creation
curl http://localhost:8080/api/transactions | jq '.transactions[-5:]'

# Check blockchain persistence
curl http://localhost:8080/api/blocks | jq '.blocks[-3:]'
```

## 🌐 GCP Terraform Infrastructure

### Infrastructure Components

The GCP node includes:

- **Compute Engine**: VM instance running blockchain peer
- **Cloud Storage**: Persistent data storage
- **VPC Network**: Secure network connectivity
- **Firewall Rules**: Access control
- **Cloud SQL**: Metadata database (optional)
- **Load Balancer**: High availability (optional)

### Terraform Commands

```bash
# Initialize and validate
terraform init
terraform validate
terraform plan

# Deploy infrastructure
terraform apply -auto-approve

# View outputs
terraform output

# Destroy when done
terraform destroy -auto-approve
```

### Multi-Cloud Connectivity

```bash
# Configure cross-cloud networking
# Update peer addresses in both nodes
# Local node: localhost:7051
# GCP node: <gcp-external-ip>:7051

# Test connectivity
docker exec cobol-metadata-node ping <gcp-peer-ip>
```

## 📊 Sample Data

The system includes comprehensive sample data:

- **MASTER.DAT**: 7 master records
- **CUSTOMER.DAT**: 5 customer profiles
- **TRANSACTION.DAT**: 5 transaction records
- **ACCOUNTS.DAT**: 5 account records
- **PRODUCTS.IDX**: 5 product entries

## 🔒 Security Considerations

- Private blockchain network with permissioned access
- TLS encryption between peers (configurable)
- API authentication (to be implemented)
- Data encryption at rest (GCP managed)
- Network segmentation via VPC

## 🚨 Troubleshooting

### Common Issues

1. **Container fails to start**
   ```bash
   # Check logs
   docker-compose logs cobol-blockchain
   
   # Restart services
   docker-compose restart
   ```

2. **COBOL compilation errors**
   ```bash
   # Check COBOL program syntax
   docker exec cobol-metadata-node cobc -x --syntax-only /app/cobol_programs/metadata-capture.cob
   ```

3. **Blockchain connection issues**
   ```bash
   # Verify peer connectivity
   docker exec peer0.org1.example.com peer channel list
   
   # Check certificates
   ls -la blockchain/crypto-config/
   ```

4. **API not responding**
   ```bash
   # Check adapter status
   docker exec cobol-metadata-node ps aux | grep python
   
   # Restart adapter
   docker exec cobol-metadata-node pkill -f "python.*adapter"
   docker exec -d cobol-metadata-node python3 /app/adapters/cobol-fabric-adapter.py
   ```

### GCP Troubleshooting

```bash
# Check VM status
gcloud compute instances list

# SSH to instance
gcloud compute ssh blockchain-node --zone=us-central1-a

# Check firewall rules
gcloud compute firewall-rules list --filter="name~blockchain"

# View logs
gcloud logging read "resource.type=gce_instance"
```

## 📈 Performance Tuning

### Local Node Optimization

- Increase Docker memory allocation (8GB recommended)
- Use SSD storage for blockchain data
- Optimize COBOL file I/O patterns
- Tune adapter polling intervals

### GCP Node Optimization

- Use Compute Engine Persistent Disk SSD
- Enable Preemptible instances for cost savings
- Configure auto-scaling for high availability
- Use Cloud CDN for static assets

## 🤝 Contributing

1. Fork the repository
2. Create a feature branch
3. Test thoroughly with both nodes
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

---

**🔗 Quick Links**
- [Dashboard](http://localhost:8080) - Real-time monitoring
- [Admin Interface](./cobol-admin.html) - Data management
- [API Documentation](http://localhost:8080/api/) - REST endpoints
- [COBOL Programs](./cobol_programs/) - Source code
- [Terraform Configs](./terraform/) - Infrastructure as code as code