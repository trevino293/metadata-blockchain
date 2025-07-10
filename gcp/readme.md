# GCP Cloud Blockchain Node

## 🎯 Overview

The GCP Cloud Blockchain Node provides a simple, robust cloud deployment for the COBOL metadata blockchain system. It uses Google Cloud Platform services including Compute Engine, Pub/Sub, and automated VM provisioning to create a production-ready blockchain node.

## 🏗️ Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                     Google Cloud Platform                   │
├─────────────────────────────────────────────────────────────┤
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────────────┐  │
│  │   Pub/Sub   │  │     VM      │  │     Firewall        │  │
│  │             │  │             │  │                     │  │
│  │ Topic:      │◄─│ Blockchain  │◄─│ Port 8080           │  │
│  │ metadata-   │  │ Node        │  │ HTTP API            │  │
│  │ events      │  │             │  │                     │  │
│  │             │  │ e2-small    │  │ External Access     │  │
│  │ Sub:        │  │ Ubuntu      │  │                     │  │
│  │ blockchain- │  │ Python      │  │                     │  │
│  │ sub         │  │             │  │                     │  │
│  └─────────────┘  └─────────────┘  └─────────────────────┘  │
│         │                │                      │           │
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────────────┐  │
│  │   Service   │  │   Startup   │  │     Monitoring      │  │
│  │   Account   │  │   Script    │  │                     │  │
│  │             │  │             │  │ Logs                │  │
│  │ Pub/Sub     │  │ Python      │  │ Status API          │  │
│  │ Publisher   │  │ HTTP Server │  │ Health Checks       │  │
│  │ Subscriber  │  │ Blockchain  │  │                     │  │
│  └─────────────┘  └─────────────┘  └─────────────────────┘  │
└─────────────────────────────────────────────────────────────┘
```

## 🚀 Quick Start

### Prerequisites

- **Google Cloud SDK** (`gcloud` CLI installed and configured)
- **PowerShell 5.1+** (or PowerShell 7+)
- **Google Cloud Project** with billing enabled
- **Required APIs**: Compute Engine, Pub/Sub

### 1. Authentication & Project Setup

```powershell
# Authenticate with Google Cloud
gcloud auth login

# Set your project ID
gcloud config set project YOUR_PROJECT_ID

# Verify authentication
gcloud auth list
gcloud config get-value project
```

### 2. Deploy Infrastructure

```powershell
# Navigate to GCP directory
cd gcp

# Deploy all resources with one command
.\simple-gcp-deploy.ps1 -ProjectId "YOUR_PROJECT_ID"

# Optional: Specify region/zone
.\simple-gcp-deploy.ps1 -ProjectId "YOUR_PROJECT_ID" -Region "us-west1" -Zone "us-west1-a"
```

### 3. Test the Deployment

```powershell
# Run comprehensive tests
.\test-simple.ps1 -ProjectId "YOUR_PROJECT_ID"

# Check deployment status
gcloud compute instances list --project=YOUR_PROJECT_ID
```

### 4. Monitor the System

```powershell
# Open generated monitoring interface
.\blockchain-monitor.html

# Check system status via API
$vmIp = "YOUR_VM_IP"  # From deployment output
Invoke-RestMethod -Uri "http://$vmIp:8080/status"
```

## 📁 Directory Structure

```
gcp/
├── simple-gcp-deploy.ps1           # Main deployment script
├── test-simple.ps1                 # Comprehensive test suite
├── debug-gcp-issues.ps1            # Debugging and troubleshooting
├── cleanup-simple.ps1              # Resource cleanup
├── connection-info.txt             # Generated connection details
├── blockchain-monitor.html         # Generated web monitoring interface
└── README.md                       # This file
```

## 🔧 Configuration

### Deployment Script Options

```powershell
.\simple-gcp-deploy.ps1 `
  -ProjectId "your-project-id" `
  -Region "us-central1" `
  -Zone "us-central1-a" `
  -VmName "blockchain-node" `
  -VmType "e2-small" `
  -Topic "metadata-events" `
  -Subscription "blockchain-sub"
```

### Resource Configuration

The deployment creates:

1. **Pub/Sub Resources**
   - Topic: `metadata-events`
   - Subscription: `blockchain-sub`
   - Message retention: 7 days

2. **Compute Instance**
   - Type: `e2-small` (2 vCPU, 2GB RAM)
   - OS: Ubuntu 20.04 LTS
   - Disk: 10GB persistent disk
   - Network: Default VPC

3. **Firewall Rules**
   - Port 8080: HTTP API access
   - Port 22: SSH access (for debugging)

4. **Service Account**
   - Pub/Sub Publisher/Subscriber
   - Compute Instance access

### VM Startup Script

The deployment automatically installs and configures:

```bash
# System updates
apt-get update && apt-get install -y python3 python3-pip jq curl

# Python dependencies
pip3 install google-cloud-pubsub

# Blockchain application
# - HTTP server for API
# - Pub/Sub message processing
# - Block creation and storage
# - JSON message handling
```

## 🖥️ Web Interface & API

### Generated Monitoring Interface

The deployment creates `blockchain-monitor.html` with:

- Real-time status dashboard
- Blockchain explorer
- Transaction monitoring
- System health indicators
- Direct API integration

### API Endpoints

Once deployed, the VM exposes these endpoints:

```powershell
# Get VM IP from deployment output
$vmIp = "YOUR_VM_IP"

# System Status
$status = Invoke-RestMethod -Uri "http://$vmIp:8080/status"
# Returns: { blocks, pending_transactions, total_messages_processed, status }

# Health Check
$health = Invoke-RestMethod -Uri "http://$vmIp:8080/health"
# Returns: { status, vm_uptime, python_version, project_id }

# Blockchain Blocks
$blocks = Invoke-RestMethod -Uri "http://$vmIp:8080/blocks"
# Returns: { total_blocks, blocks: [...] }

# Debug Information
$debug = Invoke-RestMethod -Uri "http://$vmIp:8080/debug"
# Returns: { debug_info, subscription_details, message_stats }

# Pending Transactions
$transactions = Invoke-RestMethod -Uri "http://$vmIp:8080/transactions"
# Returns: { total_pending, transactions: [...] }
```

### API Usage Examples

```powershell
# Monitor system in real-time
while ($true) {
    $status = Invoke-RestMethod -Uri "http://$vmIp:8080/status"
    Write-Host "$(Get-Date): Blocks: $($status.blocks), Messages: $($status.total_messages_processed)"
    Start-Sleep -Seconds 5
}

# Get detailed block information
$blocks = Invoke-RestMethod -Uri "http://$vmIp:8080/blocks"
$blocks.blocks | ForEach-Object {
    Write-Host "Block $($_.index): $($_.transactions.Count) transactions, Hash: $($_.hash.Substring(0,16))..."
}
```

## 🧪 Testing

### Automated Test Suite

```powershell
# Run complete test suite
.\test-simple.ps1 -ProjectId "YOUR_PROJECT_ID"

# Test output includes:
# - VM status verification
# - API endpoint testing
# - Pub/Sub message publishing
# - Blockchain creation verification
# - Performance metrics
```

### Manual Testing

```powershell
# 1. Publish test message to Pub/Sub
gcloud pubsub topics publish metadata-events `
  --message='{"operation":"CREATE","entity":"Customer","id":"TEST_001","source":"Manual"}' `
  --project=YOUR_PROJECT_ID

# 2. Wait for processing
Start-Sleep -Seconds 10

# 3. Check blockchain response
$vmIp = "YOUR_VM_IP"
$blocks = Invoke-RestMethod -Uri "http://$vmIp:8080/blocks"
Write-Host "Total blocks: $($blocks.total_blocks)"

# 4. Verify message processing
$status = Invoke-RestMethod -Uri "http://$vmIp:8080/status"
Write-Host "Messages processed: $($status.total_messages_processed)"
```

### Load Testing

```powershell
# Publish multiple messages rapidly
1..10 | ForEach-Object {
    $message = @{
        operation = "CREATE"
        entity = "LoadTest"
        id = "LOAD_TEST_$_"
        source = "LoadTest"
        timestamp = (Get-Date).ToString("yyyy-MM-ddTHH:mm:ss.fffZ")
        sequence = $_
    } | ConvertTo-Json -Compress
    
    gcloud pubsub topics publish metadata-events --message="$message" --project=YOUR_PROJECT_ID
    Write-Host "Published message $_"
}

# Monitor processing
Start-Sleep -Seconds 30
$finalStatus = Invoke-RestMethod -Uri "http://$vmIp:8080/status"
Write-Host "Final state: $($finalStatus.blocks) blocks, $($finalStatus.total_messages_processed) messages"
```

## 🔍 Monitoring & Logging

### Real-time Monitoring

```powershell
# VM system logs
gcloud compute ssh blockchain-node `
  --zone=us-central1-a `
  --command='sudo journalctl -u blockchain -f' `
  --project=YOUR_PROJECT_ID

# VM resource usage
gcloud compute ssh blockchain-node `
  --zone=us-central1-a `
  --command='top -n 1' `
  --project=YOUR_PROJECT_ID

# Network connectivity
gcloud compute ssh blockchain-node `
  --zone=us-central1-a `
  --command='netstat -tuln | grep 8080' `
  --project=YOUR_PROJECT_ID
```

### Pub/Sub Monitoring

```powershell
# Check subscription status
gcloud pubsub subscriptions describe blockchain-sub --project=YOUR_PROJECT_ID

# View message backlog
gcloud pubsub subscriptions list --project=YOUR_PROJECT_ID

# Monitor topic activity
gcloud logging read "resource.type=pubsub_topic AND resource.labels.topic_id=metadata-events" `
  --project=YOUR_PROJECT_ID `
  --limit=10
```

### Performance Metrics

```powershell
# Get VM metrics
gcloud compute instances describe blockchain-node `
  --zone=us-central1-a `
  --project=YOUR_PROJECT_ID

# Check API response times
Measure-Command { Invoke-RestMethod -Uri "http://$vmIp:8080/status" }

# Monitor blockchain growth
$blocks = Invoke-RestMethod -Uri "http://$vmIp:8080/blocks"
$blocks.blocks | Measure-Object | Select-Object Count
```

## 🚨 Troubleshooting

### Comprehensive Debug Tool

```powershell
# Run full diagnostic
.\debug-gcp-issues.ps1 -ProjectId "YOUR_PROJECT_ID"

# This checks:
# - VM status and connectivity
# - API endpoint availability
# - Pub/Sub configuration
# - Service account permissions
# - Firewall rules
# - Application logs
```

### Common Issues & Solutions

1. **VM Not Responding**
   ```powershell
   # Check VM status
   gcloud compute instances list --project=YOUR_PROJECT_ID
   
   # Restart VM if needed
   gcloud compute instances reset blockchain-node --zone=us-central1-a --project=YOUR_PROJECT_ID
   
   # Check startup script logs
   gcloud compute ssh blockchain-node --zone=us-central1-a --command='cat /var/log/startup-script.log' --project=YOUR_PROJECT_ID
   ```

2. **API Not Accessible**
   ```powershell
   # Verify firewall rule
   gcloud compute firewall-rules describe allow-blockchain --project=YOUR_PROJECT_ID
   
   # Test from VM itself
   gcloud compute ssh blockchain-node --zone=us-central1-a --command='curl localhost:8080/status' --project=YOUR_PROJECT_ID
   
   # Check application status
   gcloud compute ssh blockchain-node --zone=us-central1-a --command='sudo systemctl status blockchain' --project=YOUR_PROJECT_ID
   ```

3. **Pub/Sub Messages Not Processing**
   ```powershell
   # Check subscription backlog
   gcloud pubsub subscriptions describe blockchain-sub --project=YOUR_PROJECT_ID
   
   # Test message publishing
   gcloud pubsub topics publish metadata-events --message='{"test":"debug"}' --project=YOUR_PROJECT_ID
   
   # Check subscriber logs
   gcloud compute ssh blockchain-node --zone=us-central1-a --command='sudo journalctl -u blockchain -n 50' --project=YOUR_PROJECT_ID
   ```

4. **Authentication Issues**
   ```powershell
   # Check service account
   gcloud iam service-accounts list --project=YOUR_PROJECT_ID
   
   # Verify permissions
   gcloud projects get-iam-policy YOUR_PROJECT_ID
   
   # Test authentication
   gcloud compute ssh blockchain-node --zone=us-central1-a --command='gcloud auth list' --project=YOUR_PROJECT_ID
   ```

5. **Performance Issues**
   ```powershell
   # Check VM resources
   gcloud compute ssh blockchain-node --zone=us-central1-a --command='free -h && df -h' --project=YOUR_PROJECT_ID
   
   # Monitor CPU usage
   gcloud compute ssh blockchain-node --zone=us-central1-a --command='top -n 1' --project=YOUR_PROJECT_ID
   
   # Check application performance
   Measure-Command { Invoke-RestMethod -Uri "http://$vmIp:8080/status" }
   ```

### Debugging Steps

1. **Verify Infrastructure**
   ```powershell
   # Check all resources
   gcloud compute instances list --project=YOUR_PROJECT_ID
   gcloud pubsub topics list --project=YOUR_PROJECT_ID
   gcloud pubsub subscriptions list --project=YOUR_PROJECT_ID
   gcloud compute firewall-rules list --filter="name:allow-blockchain" --project=YOUR_PROJECT_ID
   ```

2. **Test Connectivity**
   ```powershell
   # Get VM external IP
   $vmIp = gcloud compute instances describe blockchain-node --zone=us-central1-a --format="value(networkInterfaces[0].accessConfigs[0].natIP)" --project=YOUR_PROJECT_ID
   
   # Test basic connectivity
   Test-NetConnection -ComputerName $vmIp -Port 8080
   
   # Test API endpoints
   try {
       Invoke-RestMethod -Uri "http://$vmIp:8080/health" -TimeoutSec 10
       Write-Host "✅ API accessible"
   } catch {
       Write-Host "❌ API not accessible: $($_.Exception.Message)"
   }
   ```

3. **Application Debugging**
   ```powershell
   # SSH into VM for detailed debugging
   gcloud compute ssh blockchain-node --zone=us-central1-a --project=YOUR_PROJECT_ID
   
   # On VM:
   # sudo journalctl -u blockchain -f     # Follow logs
   # sudo systemctl status blockchain     # Service status
   # curl localhost:8080/debug           # Application debug info
   # ps aux | grep python                # Process status
   ```

## 💰 Cost Management

### Cost Breakdown

**Monthly costs (approximate):**
- **VM (e2-small)**: ~$15 USD
- **Persistent disk (10GB)**: ~$0.40 USD
- **Pub/Sub**: $0.40 per million messages (minimal for testing)
- **Network egress**: Minimal for API calls
- **Total**: ~$15-20 USD/month

### Cost Optimization

```powershell
# Stop VM when not needed
gcloud compute instances stop blockchain-node --zone=us-central1-a --project=YOUR_PROJECT_ID

# Start when needed
gcloud compute instances start blockchain-node --zone=us-central1-a --project=YOUR_PROJECT_ID

# Use preemptible instances (cheaper, but can be terminated)
# Add --preemptible flag to deployment script

# Monitor costs
gcloud billing budgets list --billing-account=YOUR_BILLING_ACCOUNT
```

### Resource Cleanup

```powershell
# Clean up all resources
.\cleanup-simple.ps1 -ProjectId "YOUR_PROJECT_ID"

# Manual cleanup if needed
gcloud compute instances delete blockchain-node --zone=us-central1-a --project=YOUR_PROJECT_ID
gcloud pubsub subscriptions delete blockchain-sub --project=YOUR_PROJECT_ID
gcloud pubsub topics delete metadata-events --project=YOUR_PROJECT_ID
gcloud compute firewall-rules delete allow-blockchain --project=YOUR_PROJECT_ID
```

## 🔧 Advanced Configuration

### Custom VM Configuration

```powershell
# Deploy with custom VM specs
.\simple-gcp-deploy.ps1 `
  -ProjectId "YOUR_PROJECT_ID" `
  -VmType "e2-medium" `
  -DiskSize "20GB" `
  -Region "us-west1" `
  -Zone "us-west1-a"
```

### Production Deployment

For production use, consider:

1. **High Availability**
   ```powershell
   # Deploy multiple instances across zones
   .\simple-gcp-deploy.ps1 -ProjectId "YOUR_PROJECT_ID" -Zone "us-central1-a"
   .\simple-gcp-deploy.ps1 -ProjectId "YOUR_PROJECT_ID" -Zone "us-central1-b" -VmName "blockchain-node-2"
   ```

2. **Load Balancing**
   ```powershell
   # Create load balancer
   gcloud compute instance-groups unmanaged create blockchain-group --zone=us-central1-a --project=YOUR_PROJECT_ID
   gcloud compute backend-services create blockchain-backend --global --project=YOUR_PROJECT_ID
   ```

3. **Security Hardening**
   ```powershell
   # Restrict firewall to specific IPs
   gcloud compute firewall-rules update allow-blockchain --source-ranges="YOUR_IP/32" --project=YOUR_PROJECT_ID
   
   # Use private networks
   gcloud compute networks create blockchain-vpc --project=YOUR_PROJECT_ID
   ```

### Integration with Local Node

```powershell
# Configure local node to publish to GCP
# In docker-cobol-blockchain/adapters/config/adapter_config.json:
{
  "gcp": {
    "project_id": "YOUR_PROJECT_ID",
    "pubsub_topic": "metadata-events",
    "credentials_path": "/app/config/gcp-credentials.json"
  }
}

# Test hybrid deployment
cd ../docker-cobol-blockchain
.\tests\test-cobol-blockchain.ps1 -GcpProjectId "YOUR_PROJECT_ID"
```

## 📈 Scaling Considerations

### Horizontal Scaling

```powershell
# Create multiple VMs for load distribution
1..3 | ForEach-Object {
    .\simple-gcp-deploy.ps1 -ProjectId "YOUR_PROJECT_ID" -VmName "blockchain-node-$_" -Zone "us-central1-$("abc"[$_-1])"
}
```

### Vertical Scaling

```powershell
# Resize VM for higher load
gcloud compute instances set-machine-type blockchain-node --machine-type=e2-medium --zone=us-central1-a --project=YOUR_PROJECT_ID
```

### Message Processing Optimization

```python
# Modify blockchain application for batch processing
# Increase batch_size in startup script
# Add message queuing and parallel processing
```

## 🔗 Integration Examples

### Webhook Integration

```powershell
# Configure webhook endpoint for external systems
curl -X POST "http://$vmIp:8080/webhook" `
  -H "Content-Type: application/json" `
  -d '{"operation":"CREATE","entity":"WebhookTest","id":"WH_001"}'
```

### Monitoring Integration

```powershell
# Integrate with Google Cloud Monitoring
gcloud logging create blockchain-logs --project=YOUR_PROJECT_ID

# Set up alerting
gcloud alpha monitoring policies create --policy-from-file=alert-policy.yaml --project=YOUR_PROJECT_ID
```

## 📚 References

- [Google Cloud Compute Engine](https://cloud.google.com/compute)
- [Google Cloud Pub/Sub](https://cloud.google.com/pubsub)
- [Google Cloud SDK](https://cloud.google.com/sdk)
- [Main Project README](../README.md)
- [Docker COBOL README](../docker-cobol-blockchain/README.md)

---

**Next Steps:**
1. Review [Main Project README](../README.md) for system overview
2. Check [Docker COBOL README](../docker-cobol-blockchain/README.md) for local deployment
3. See [Testing Documentation](./tests/README.md) for detailed testing procedures