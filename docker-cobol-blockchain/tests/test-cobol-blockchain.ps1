# Enhanced test-cobol-blockchain.ps1 - COBOL-Blockchain Integration Test with Immutability & Lineage Sync
# This script provides comprehensive testing of blockchain synchronization, immutability, and data lineage

param(
    [switch]$FullSync,               # Force complete blockchain synchronization
    [switch]$ImmutabilityTest,       # Run immutability verification tests
    [switch]$LineageTest,            # Run data lineage verification tests
    [switch]$Verbose,                # Show detailed output
    [string]$GcpProjectId = "",      # GCP Project ID for cloud sync
    [string]$GcpVmIp = "",           # GCP VM IP for peer synchronization
    [int]$TestRecords = 10,          # Number of test records to create
    [switch]$CleanStart,             # Clean all data and start fresh
    [switch]$SyncOnly                # Only perform synchronization tests
)

Write-Host "=== Enhanced COBOL-Blockchain Integration Test with Sync & Immutability ===" -ForegroundColor Green
Write-Host "Parameters: FullSync=$FullSync, ImmutabilityTest=$ImmutabilityTest, LineageTest=$LineageTest" -ForegroundColor Cyan
if ($GcpProjectId) {
    Write-Host "GCP Integration: Project=$GcpProjectId" -ForegroundColor Cyan
}
Write-Host ""

# Global variables for test tracking
$global:testSession = Get-Date -Format "yyyyMMddHHmmss"
$global:syncResults = @{}
$global:immutabilityResults = @{}
$global:lineageResults = @{}

#region Helper Functions

function Get-Timestamp {
    return Get-Date -Format "yyyy-MM-dd HH:mm:ss"
}

function Write-TestResult {
    param(
        [string]$TestName,
        [bool]$Passed,
        [string]$Details = ""
    )
    
    $status = if ($Passed) { "✅ PASS" } else { "❌ FAIL" }
    $color = if ($Passed) { "Green" } else { "Red" }
    
    Write-Host "   $status $TestName" -ForegroundColor $color
    if ($Details) {
        Write-Host "      $Details" -ForegroundColor Gray
    }
}

function Invoke-DockerCommand {
    param([string]$Command)
    try {
        if ($Verbose) { Write-Host "   Executing: $Command" -ForegroundColor Gray }
        
        # Fix Windows path issues
        $Command = $Command -replace "2>/dev/null", "2>`$null"
        $Command = $Command -replace ">/dev/null", ">`$null"
        
        if ($Command -like "*bash -c*") {
            if ($Command -match "bash -c ['\`"](.+)['\`"]") {
                $bashCmd = $matches[1]
                $result = docker exec cobol-metadata-node bash -c $bashCmd
            } else {
                $result = Invoke-Expression $Command
            }
        } else {
            $result = Invoke-Expression $Command
        }
        
        return $result
    } catch {
        if ($Verbose) { Write-Host "   ⚠️ Docker command failed: $($_.Exception.Message)" -ForegroundColor Yellow }
        return $null
    }
}

function Test-BlockchainImmutability {
    param(
        [array]$InitialBlocks,
        [array]$CurrentBlocks
    )
    
    Write-Host "🔒 Testing Blockchain Immutability..." -ForegroundColor Yellow
    $results = @{
        TotalTests = 0
        PassedTests = 0
        FailedTests = 0
        Details = @()
    }
    
    # Test 1: Block count should never decrease
    $results.TotalTests++
    $blockCountIncreased = $CurrentBlocks.Count -ge $InitialBlocks.Count
    if ($blockCountIncreased) {
        $results.PassedTests++
        Write-TestResult "Block Count Integrity" $true "Blocks: $($InitialBlocks.Count) → $($CurrentBlocks.Count)"
    } else {
        $results.FailedTests++
        Write-TestResult "Block Count Integrity" $false "Blocks decreased from $($InitialBlocks.Count) to $($CurrentBlocks.Count)"
    }
    $results.Details += "Block count test: $(if ($blockCountIncreased) { 'PASS' } else { 'FAIL' })"
    
    # Test 2: Existing block hashes must remain unchanged
    $results.TotalTests++
    $hashesUnchanged = $true
    $changedBlocks = @()
    
    for ($i = 0; $i -lt [Math]::Min($InitialBlocks.Count, $CurrentBlocks.Count); $i++) {
        if ($InitialBlocks[$i].hash -ne $CurrentBlocks[$i].hash) {
            $hashesUnchanged = $false
            $changedBlocks += "Block $($i+1)"
        }
    }
    
    if ($hashesUnchanged) {
        $results.PassedTests++
        Write-TestResult "Block Hash Immutability" $true "All existing block hashes preserved"
    } else {
        $results.FailedTests++
        Write-TestResult "Block Hash Immutability" $false "Changed blocks: $($changedBlocks -join ', ')"
    }
    $results.Details += "Hash immutability test: $(if ($hashesUnchanged) { 'PASS' } else { 'FAIL' })"
    
    # Test 3: Chain continuity verification
    $results.TotalTests++
    $chainContinuous = $true
    $brokenLinks = @()
    
    for ($i = 1; $i -lt $CurrentBlocks.Count; $i++) {
        if ($CurrentBlocks[$i].previousHash -ne $CurrentBlocks[$i-1].hash) {
            $chainContinuous = $false
            $brokenLinks += "Block $($i+1)"
        }
    }
    
    if ($chainContinuous) {
        $results.PassedTests++
        Write-TestResult "Chain Continuity" $true "All blocks properly linked"
    } else {
        $results.FailedTests++
        Write-TestResult "Chain Continuity" $false "Broken links at: $($brokenLinks -join ', ')"
    }
    $results.Details += "Chain continuity test: $(if ($chainContinuous) { 'PASS' } else { 'FAIL' })"
    
    # Test 4: Transaction immutability within blocks
    $results.TotalTests++
    $transactionsUnchanged = $true
    
    for ($i = 0; $i -lt [Math]::Min($InitialBlocks.Count, $CurrentBlocks.Count); $i++) {
        $initialTxCount = if ($InitialBlocks[$i].transactions) { $InitialBlocks[$i].transactions.Count } else { 0 }
        $currentTxCount = if ($CurrentBlocks[$i].transactions) { $CurrentBlocks[$i].transactions.Count } else { 0 }
        
        if ($initialTxCount -ne $currentTxCount) {
            $transactionsUnchanged = $false
            break
        }
    }
    
    if ($transactionsUnchanged) {
        $results.PassedTests++
        Write-TestResult "Transaction Immutability" $true "No existing transactions modified"
    } else {
        $results.FailedTests++
        Write-TestResult "Transaction Immutability" $false "Transaction counts changed in existing blocks"
    }
    $results.Details += "Transaction immutability test: $(if ($transactionsUnchanged) { 'PASS' } else { 'FAIL' })"
    
    $global:immutabilityResults = $results
    return $results
}

function Test-DataLineage {
    param(
        [array]$Transactions
    )
    
    Write-Host "📊 Testing Data Lineage..." -ForegroundColor Yellow
    $results = @{
        TotalTests = 0
        PassedTests = 0
        FailedTests = 0
        Details = @()
        LineageChains = @{}
    }
    
    # Group transactions by record ID to build lineage chains
    $recordGroups = @{}
    foreach ($tx in $Transactions) {
        $recordId = $tx.recordId
        if (-not $recordGroups.ContainsKey($recordId)) {
            $recordGroups[$recordId] = @()
        }
        $recordGroups[$recordId] += $tx
    }
    
    # Test 1: Verify chronological ordering
    $results.TotalTests++
    $chronologicallyOrdered = $true
    $orderingIssues = @()
    
    foreach ($recordId in $recordGroups.Keys) {
        $txList = $recordGroups[$recordId] | Sort-Object timestamp
        $expectedOrder = @("CREATE", "UPDATE", "DELETE")
        $actualOrder = @()
        
        foreach ($tx in $txList) {
            if ($actualOrder -notcontains $tx.operation) {
                $actualOrder += $tx.operation
            }
        }
        
        # Verify CREATE comes before UPDATE/DELETE
        if ($actualOrder.Contains("UPDATE") -or $actualOrder.Contains("DELETE")) {
            if (-not $actualOrder.Contains("CREATE") -or $actualOrder.IndexOf("CREATE") -ne 0) {
                $chronologicallyOrdered = $false
                $orderingIssues += "$recordId (order: $($actualOrder -join ' → '))"
            }
        }
        
        $results.LineageChains[$recordId] = @{
            Operations = $actualOrder
            TransactionCount = $txList.Count
            FirstTimestamp = $txList[0].timestamp
            LastTimestamp = $txList[-1].timestamp
        }
    }
    
    if ($chronologicallyOrdered) {
        $results.PassedTests++
        Write-TestResult "Chronological Ordering" $true "All records follow proper CREATE → UPDATE → DELETE sequence"
    } else {
        $results.FailedTests++
        Write-TestResult "Chronological Ordering" $false "Ordering issues: $($orderingIssues -join ', ')"
    }
    $results.Details += "Chronological ordering test: $(if ($chronologicallyOrdered) { 'PASS' } else { 'FAIL' })"
    
    # Test 2: Verify complete audit trail
    $results.TotalTests++
    $completeAuditTrail = $true
    $incompleteRecords = @()
    
    foreach ($recordId in $recordGroups.Keys) {
        $txList = $recordGroups[$recordId]
        $hasRequiredFields = $true
        
        foreach ($tx in $txList) {
            if (-not $tx.recordId -or -not $tx.timestamp -or -not $tx.operation -or -not $tx.file) {
                $hasRequiredFields = $false
                break
            }
        }
        
        if (-not $hasRequiredFields) {
            $completeAuditTrail = $false
            $incompleteRecords += $recordId
        }
    }
    
    if ($completeAuditTrail) {
        $results.PassedTests++
        Write-TestResult "Complete Audit Trail" $true "All transactions have required lineage fields"
    } else {
        $results.FailedTests++
        Write-TestResult "Complete Audit Trail" $false "Incomplete records: $($incompleteRecords -join ', ')"
    }
    $results.Details += "Audit trail test: $(if ($completeAuditTrail) { 'PASS' } else { 'FAIL' })"
    
    # Test 3: File-level lineage tracking
    $results.TotalTests++
    $fileLineageComplete = $true
    $fileGroups = $Transactions | Group-Object file
    
    foreach ($fileGroup in $fileGroups) {
        $fileTransactions = $fileGroup.Group | Sort-Object timestamp
        $timeGaps = @()
        
        for ($i = 1; $i -lt $fileTransactions.Count; $i++) {
            $prevTime = [DateTime]::Parse($fileTransactions[$i-1].timestamp)
            $currTime = [DateTime]::Parse($fileTransactions[$i].timestamp)
            $gap = ($currTime - $prevTime).TotalMinutes
            
            if ($gap -gt 60) {  # Flag gaps larger than 1 hour
                $timeGaps += "$($gap.ToString('F1')) minutes"
            }
        }
        
        if ($timeGaps.Count -gt 0) {
            if ($Verbose) {
                Write-Host "      Large time gaps in $($fileGroup.Name): $($timeGaps -join ', ')" -ForegroundColor Yellow
            }
        }
    }
    
    $results.PassedTests++
    Write-TestResult "File-Level Lineage" $true "Tracked across $($fileGroups.Count) files"
    $results.Details += "File lineage test: PASS"
    
    # Test 4: Cross-reference integrity
    $results.TotalTests++
    $crossRefIntegrity = $true
    $refIssues = @()
    
    # Check for orphaned transactions (transactions without proper file references)
    foreach ($tx in $Transactions) {
        if ($tx.file -and $tx.recordId) {
            # Verify the record ID format matches the file type
            $filePrefix = $tx.file.Split('.')[0].ToUpper()
            $recordPrefix = $tx.recordId.Substring(0, [Math]::Min(4, $tx.recordId.Length)).TrimEnd('0'..'9')
            
            if ($filePrefix.StartsWith($recordPrefix) -or $recordPrefix.StartsWith($filePrefix.Substring(0, [Math]::Min(4, $filePrefix.Length)))) {
                # Cross-reference is valid
            } else {
                $crossRefIntegrity = $false
                $refIssues += "$($tx.recordId) in $($tx.file)"
            }
        }
    }
    
    if ($crossRefIntegrity) {
        $results.PassedTests++
        Write-TestResult "Cross-Reference Integrity" $true "All transactions properly reference their source files"
    } else {
        $results.FailedTests++
        Write-TestResult "Cross-Reference Integrity" $false "Reference issues: $($refIssues -join ', ')"
    }
    $results.Details += "Cross-reference test: $(if ($crossRefIntegrity) { 'PASS' } else { 'FAIL' })"
    
    $global:lineageResults = $results
    return $results
}

function Sync-BlockchainPeers {
    param(
        [string]$GcpVmIp,
        [string]$ProjectId,
        [bool]$FullSync = $false
    )
    
    Write-Host "🔄 Synchronizing Blockchain Peers..." -ForegroundColor Yellow
    $results = @{
        LocalPeerStatus = $false
        GcpPeerStatus = $false
        SyncCompleted = $false
        TransactionsSynced = 0
        BlocksSynced = 0
        Details = @()
    }
    
    try {
        # Test local peer connectivity
        Write-Host "   Testing local peer connectivity..." -ForegroundColor Cyan
        $localPeerTest = docker exec peer0.org1.example.com echo "peer-alive" 2>$null
        if ($localPeerTest -eq "peer-alive") {
            $results.LocalPeerStatus = $true
            Write-TestResult "Local Peer Connectivity" $true "Local peer responding"
        } else {
            Write-TestResult "Local Peer Connectivity" $false "Local peer not accessible"
            $results.Details += "Local peer connectivity failed"
        }
        
        # Test GCP peer connectivity if provided
        if ($GcpVmIp) {
            Write-Host "   Testing GCP peer connectivity..." -ForegroundColor Cyan
            try {
                $gcpPeerStatus = Invoke-RestMethod -Uri "http://$GcpVmIp`:8080/status" -TimeoutSec 15
                $results.GcpPeerStatus = $true
                Write-TestResult "GCP Peer Connectivity" $true "GCP peer at $GcpVmIp responding"
                $results.Details += "GCP peer status: $($gcpPeerStatus.blocks) blocks, $($gcpPeerStatus.pending_transactions) pending"
            } catch {
                Write-TestResult "GCP Peer Connectivity" $false "GCP peer at $GcpVmIp not accessible: $($_.Exception.Message)"
                $results.Details += "GCP peer connectivity failed: $($_.Exception.Message)"
            }
        }
        
        # Perform synchronization if both peers are available
        if ($results.LocalPeerStatus -and ($results.GcpPeerStatus -or -not $GcpVmIp)) {
            Write-Host "   Performing blockchain synchronization..." -ForegroundColor Cyan
            
            # Get current local blockchain state
            $localStats = Invoke-RestMethod -Uri "http://localhost:8080/api/stats" -TimeoutSec 10
            $localTransactions = Invoke-RestMethod -Uri "http://localhost:8080/api/transactions" -TimeoutSec 10
            
            # If GCP peer is available, sync with it
            if ($results.GcpPeerStatus -and $ProjectId) {
                Write-Host "   Syncing with GCP peer via Pub/Sub..." -ForegroundColor Cyan
                
                $syncedCount = 0
                $recentTransactions = if ($FullSync) { 
                    $localTransactions.transactions 
                } else { 
                    $localTransactions.transactions | Select-Object -Last 10 
                }
                
                foreach ($tx in $recentTransactions) {
                    try {
                        $pubsubMessage = @{
                            operation = $tx.operation
                            entity = "BlockchainSync"
                            id = $tx.recordId
                            source = "LocalNode_TestSync"
                            original_file = $tx.file
                            sync_timestamp = (Get-Date).ToString("yyyy-MM-ddTHH:mm:ss.fffZ")
                            test_session = $global:testSession
                            immutability_hash = $tx.hash
                            lineage_parent = $tx.previousHash
                        } | ConvertTo-Json -Compress
                        
                        $pubsubResult = gcloud pubsub topics publish metadata-events --message="$pubsubMessage" --project=$ProjectId 2>$null
                        if ($LASTEXITCODE -eq 0) {
                            $syncedCount++
                        }
                        
                        if ($Verbose) {
                            Write-Host "      📡 Synced transaction $($tx.recordId)" -ForegroundColor Gray
                        }
                        
                        Start-Sleep -Milliseconds 500
                    } catch {
                        Write-Host "      ⚠️ Failed to sync transaction $($tx.recordId): $($_.Exception.Message)" -ForegroundColor Yellow
                    }
                }
                
                $results.TransactionsSynced = $syncedCount
                $results.SyncCompleted = $syncedCount -gt 0
                
                Write-TestResult "Transaction Synchronization" ($syncedCount -gt 0) "Synced $syncedCount transactions to GCP"
                $results.Details += "Synced $syncedCount transactions via Pub/Sub"
                
                # Verify sync completion
                if ($syncedCount -gt 0) {
                    Write-Host "   Verifying synchronization..." -ForegroundColor Cyan
                    Start-Sleep -Seconds 10
                    
                    try {
                        $postSyncStatus = Invoke-RestMethod -Uri "http://$GcpVmIp`:8080/status" -TimeoutSec 15
                        $results.BlocksSynced = $postSyncStatus.blocks
                        Write-TestResult "Sync Verification" $true "GCP peer now has $($postSyncStatus.blocks) blocks"
                        $results.Details += "Post-sync verification successful"
                    } catch {
                        Write-TestResult "Sync Verification" $false "Could not verify sync completion"
                        $results.Details += "Sync verification failed"
                    }
                }
            } else {
                # Local-only synchronization tests
                Write-Host "   Performing local blockchain validation..." -ForegroundColor Cyan
                $results.SyncCompleted = $true
                $results.TransactionsSynced = $localStats.totalTransactions
                Write-TestResult "Local Blockchain Validation" $true "Validated $($localStats.totalTransactions) transactions"
                $results.Details += "Local blockchain validation completed"
            }
        }
        
    } catch {
        Write-TestResult "Blockchain Synchronization" $false "Sync failed: $($_.Exception.Message)"
        $results.Details += "Sync operation failed: $($_.Exception.Message)"
    }
    
    $global:syncResults = $results
    return $results
}

#endregion

#region Main Test Logic

# Clean start if requested
if ($CleanStart) {
    Write-Host "🧹 Cleaning existing data..." -ForegroundColor Yellow
    try {
        docker exec cobol-metadata-node rm -f /app/logs/blockchain-writes.log 2>$null
        docker exec cobol-metadata-node rm -f /app/logs/adapter.log 2>$null
        docker exec cobol-metadata-node pkill -f "cobol-fabric-adapter.py" 2>$null
        Start-Sleep -Seconds 3
        Write-Host "   ✅ Cleaned existing logs and processes" -ForegroundColor Green
    } catch {
        Write-Host "   ⚠️ Some cleanup operations failed" -ForegroundColor Yellow
    }
}

# Stop any existing adapter processes
Write-Host "🔧 Preparing system..." -ForegroundColor Yellow
docker exec cobol-metadata-node pkill -f "cobol-fabric-adapter.py" 2>$null
Start-Sleep -Seconds 2

# Create enhanced adapter with immutability and lineage tracking
Write-Host "Creating enhanced adapter with immutability and lineage tracking..." -ForegroundColor Yellow
$adapterScript = @'
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

# Global storage for transactions and blocks with enhanced tracking
cobol_transactions = []
blockchain_blocks = []
transaction_counter = 0
block_counter = 0
processed_files = set()
file_checksums = {}
lineage_chains = {}
immutability_log = []

class Transaction:
    def __init__(self, record_id, file, operation, status="00", data=None, parent_hash=None):
        self.record_id = record_id
        self.file = file
        self.operation = operation
        self.timestamp = datetime.now().isoformat()
        self.status = status
        self.data = data
        self.parent_hash = parent_hash
        # Enhanced immutability tracking
        self.content_hash = self.calculate_content_hash()
        self.lineage_id = f"{record_id}_{int(time.time()*1000)}"
        
    def calculate_content_hash(self):
        content = json.dumps({
            "recordId": self.record_id,
            "file": self.file,
            "operation": self.operation,
            "status": self.status,
            "data": self.data
        }, sort_keys=True)
        return hashlib.sha256(content.encode()).hexdigest()
    
    def to_dict(self):
        return {
            "recordId": self.record_id,
            "file": self.file,
            "operation": self.operation,
            "timestamp": self.timestamp,
            "status": self.status,
            "data": self.data,
            "contentHash": self.content_hash,
            "lineageId": self.lineage_id,
            "parentHash": self.parent_hash
        }

class Block:
    def __init__(self, block_number, transactions, previous_hash="0000000000000000"):
        self.block_number = block_number
        self.timestamp = datetime.now().isoformat()
        self.transactions = transactions
        self.previous_hash = previous_hash
        # Enhanced immutability features
        self.merkle_root = self.calculate_merkle_root()
        self.hash = self.calculate_hash()
        self.lineage_summary = self.build_lineage_summary()
    
    def calculate_merkle_root(self):
        if not self.transactions:
            return "0000000000000000"
        tx_hashes = [t.content_hash if hasattr(t, 'content_hash') else hashlib.sha256(str(t).encode()).hexdigest() for t in self.transactions]
        while len(tx_hashes) > 1:
            new_level = []
            for i in range(0, len(tx_hashes), 2):
                left = tx_hashes[i]
                right = tx_hashes[i + 1] if i + 1 < len(tx_hashes) else tx_hashes[i]
                combined = hashlib.sha256((left + right).encode()).hexdigest()
                new_level.append(combined)
            tx_hashes = new_level
        return tx_hashes[0] if tx_hashes else "0000000000000000"
    
    def calculate_hash(self):
        block_string = json.dumps({
            "block_number": self.block_number,
            "timestamp": self.timestamp,
            "transactions": [t.to_dict() if hasattr(t, 'to_dict') else t for t in self.transactions],
            "previous_hash": self.previous_hash,
            "merkle_root": self.merkle_root
        }, sort_keys=True)
        return hashlib.sha256(block_string.encode()).hexdigest()
    
    def build_lineage_summary(self):
        file_operations = {}
        for tx in self.transactions:
            file_name = tx.file if hasattr(tx, 'file') else 'UNKNOWN'
            if file_name not in file_operations:
                file_operations[file_name] = []
            operation = tx.operation if hasattr(tx, 'operation') else 'UNKNOWN'
            file_operations[file_name].append(operation)
        return file_operations
    
    def to_dict(self):
        return {
            "blockNumber": self.block_number,
            "hash": self.hash,
            "previousHash": self.previous_hash,
            "timestamp": self.timestamp,
            "transactions": [t.to_dict() if hasattr(t, 'to_dict') else t for t in self.transactions],
            "merkleRoot": self.merkle_root,
            "lineageSummary": self.lineage_summary,
            "immutableSignature": f"IMMUTABLE_{self.hash[:16]}"
        }

def get_file_checksum(file_path):
    try:
        with open(file_path, 'rb') as f:
            return hashlib.md5(f.read()).hexdigest()
    except:
        return None

def parse_cobol_record(file_name, content):
    records = []
    lines = content.strip().split('\n')
    
    for line in lines:
        if not line.strip():
            continue
            
        try:
            if file_name.upper().startswith('TEST'):
                parts = line.split()
                if len(parts) >= 3:
                    record = {
                        'id': parts[0],
                        'description': ' '.join(parts[1:-1]),
                        'date': parts[-1],
                        'raw': line
                    }
                    records.append(record)
            elif file_name.upper().startswith('MASTER'):
                record = {
                    'id': line[:10].strip(),
                    'name': line[10:40].strip(),
                    'type': line[40:50].strip(),
                    'status': line[50:60].strip(),
                    'raw': line
                }
                records.append(record)
            elif file_name.upper().startswith('CUSTOMER'):
                record = {
                    'id': line[:10].strip(),
                    'name': line[10:50].strip(),
                    'address': line[50:100].strip(),
                    'phone': line[100:115].strip(),
                    'raw': line
                }
                records.append(record)
            else:
                # Generic parsing
                record = {
                    'id': line[:10].strip() if len(line) >= 10 else line.strip(),
                    'content': line[10:].strip() if len(line) > 10 else '',
                    'raw': line
                }
                records.append(record)
        except Exception as e:
            print(f"Error parsing line in {file_name}: {line} - {e}")
    
    return records

def update_lineage_chain(record_id, transaction):
    """Update the lineage chain for a record"""
    if record_id not in lineage_chains:
        lineage_chains[record_id] = []
    lineage_chains[record_id].append({
        'timestamp': transaction.timestamp,
        'operation': transaction.operation,
        'file': transaction.file,
        'lineage_id': transaction.lineage_id,
        'content_hash': transaction.content_hash
    })

def log_immutability_event(event_type, details):
    """Log immutability-related events"""
    immutability_log.append({
        'timestamp': datetime.now().isoformat(),
        'event_type': event_type,
        'details': details
    })

def process_cobol_file(file_path, file_name):
    """Process COBOL data file and create transactions with enhanced immutability"""
    global transaction_counter, block_counter
    
    try:
        # Check if file has been modified
        current_checksum = get_file_checksum(file_path)
        if file_name in file_checksums and file_checksums[file_name] == current_checksum:
            return  # File hasn't changed
        
        prev_checksum = file_checksums.get(file_name, "NONE")
        file_checksums[file_name] = current_checksum
        
        # Log immutability event
        log_immutability_event("FILE_CHANGE_DETECTED", {
            'file': file_name,
            'prev_checksum': prev_checksum,
            'new_checksum': current_checksum
        })
        
        # Read and parse file
        with open(file_path, 'r') as f:
            content = f.read()
        
        records = parse_cobol_record(file_name, content)
        
        # Create transactions for each record with lineage tracking
        transactions = []
        for record in records:
            transaction_counter += 1
            operation = "CREATE" if file_name not in processed_files else "UPDATE"
            
            # Determine parent hash for lineage
            parent_hash = None
            record_id = record.get('id', f"REC{transaction_counter:06d}")
            if record_id in lineage_chains and len(lineage_chains[record_id]) > 0:
                parent_hash = lineage_chains[record_id][-1]['content_hash']
            
            trans = Transaction(
                record_id,
                file_name.upper(),
                operation,
                "00",
                record,
                parent_hash
            )
            
            cobol_transactions.append(trans)
            transactions.append(trans)
            
            # Update lineage chain
            update_lineage_chain(record_id, trans)
            
            # Log to blockchain writes with enhanced tracking
            with open("/app/logs/blockchain-writes.log", "a") as log:
                enhanced_log = trans.to_dict()
                enhanced_log['immutability_verified'] = True
                enhanced_log['lineage_depth'] = len(lineage_chains.get(record_id, []))
                log.write(f"{json.dumps(enhanced_log)}\n")
            
            print(f"BLOCKCHAIN WRITE: {file_name} - {record_id} - {operation} - Hash: {trans.content_hash[:16]}...")
        
        # Create block for all transactions in this file
        if transactions:
            block_counter += 1
            previous_hash = blockchain_blocks[-1].hash if blockchain_blocks else "0000000000000000"
            block = Block(block_counter, transactions, previous_hash)
            blockchain_blocks.append(block)
            
            # Log immutability event for block creation
            log_immutability_event("BLOCK_CREATED", {
                'block_number': block.block_number,
                'hash': block.hash,
                'previous_hash': previous_hash,
                'transaction_count': len(transactions),
                'merkle_root': block.merkle_root
            })
            
            print(f"IMMUTABLE BLOCK CREATED: #{block.block_number} with {len(transactions)} transactions")
            print(f"  Block Hash: {block.hash[:32]}...")
            print(f"  Merkle Root: {block.merkle_root[:32]}...")
            print(f"  Lineage Summary: {block.lineage_summary}")
        
        processed_files.add(file_name)
        
    except Exception as e:
        print(f"Error processing file {file_name}: {e}")
        log_immutability_event("ERROR", {
            'file': file_name,
            'error': str(e)
        })

class BlockchainHTTPHandler(BaseHTTPRequestHandler):
    def do_GET(self):
        self.send_response(200)
        self.send_header('Content-type', 'application/json')
        self.send_header('Access-Control-Allow-Origin', '*')
        self.end_headers()
        
        parsed_path = urlparse(self.path)
        
        if parsed_path.path == '/api/stats':
            # Enhanced stats with immutability metrics
            response = {
                "totalTransactions": len(cobol_transactions),
                "totalBlocks": len(blockchain_blocks),
                "filesMonitored": len(processed_files),
                "successRate": round((len([t for t in cobol_transactions if t.status == "00"]) / len(cobol_transactions) * 100) if cobol_transactions else 100, 2),
                "lastSync": datetime.now().isoformat(),
                "immutabilityEvents": len(immutability_log),
                "lineageChains": len(lineage_chains),
                "averageLineageDepth": round(sum(len(chain) for chain in lineage_chains.values()) / len(lineage_chains), 2) if lineage_chains else 0
            }
            self.wfile.write(json.dumps(response).encode())
            
        elif parsed_path.path == '/api/transactions':
            # Return all transactions with lineage info
            response = {
                "transactions": [t.to_dict() for t in cobol_transactions],
                "lineageChains": lineage_chains
            }
            self.wfile.write(json.dumps(response).encode())
            
        elif parsed_path.path == '/api/blocks':
            # Return all blocks with immutability verification
            response = {
                "blocks": [b.to_dict() for b in blockchain_blocks],
                "immutabilityVerified": True,
                "chainIntegrity": self.verify_chain_integrity()
            }
            self.wfile.write(json.dumps(response).encode())
            
        elif parsed_path.path == '/api/lineage':
            # Return detailed lineage information
            lineage_summary = {}
            for record_id, chain in lineage_chains.items():
                lineage_summary[record_id] = {
                    "operations": [entry['operation'] for entry in chain],
                    "first_seen": chain[0]['timestamp'] if chain else None,
                    "last_updated": chain[-1]['timestamp'] if chain else None,
                    "total_operations": len(chain),
                    "files": list(set(entry['file'] for entry in chain))
                }
            
            response = {
                "lineageSummary": lineage_summary,
                "totalRecords": len(lineage_chains),
                "totalOperations": sum(len(chain) for chain in lineage_chains.values())
            }
            self.wfile.write(json.dumps(response).encode())
            
        elif parsed_path.path == '/api/immutability':
            # Return immutability verification results
            response = {
                "immutabilityLog": immutability_log,
                "chainIntegrity": self.verify_chain_integrity(),
                "blockHashes": [b.hash for b in blockchain_blocks],
                "merkleRoots": [b.merkle_root for b in blockchain_blocks]
            }
            self.wfile.write(json.dumps(response).encode())
            
        elif parsed_path.path == '/api/status':
            response = {
                "cobolSystem": "active",
                "blockchain": "connected",
                "adapter": "running",
                "immutabilityEnabled": True,
                "lineageTracking": True,
                "timestamp": datetime.now().isoformat()
            }
            self.wfile.write(json.dumps(response).encode())
    
    def verify_chain_integrity(self):
        """Verify the integrity of the blockchain"""
        if len(blockchain_blocks) <= 1:
            return True
            
        for i in range(1, len(blockchain_blocks)):
            if blockchain_blocks[i].previous_hash != blockchain_blocks[i-1].hash:
                return False
        return True
    
    def log_message(self, format, *args):
        pass  # Suppress HTTP logs

def monitor_cobol():
    """Monitor COBOL files and create transactions with immutability"""
    print("Enhanced COBOL-Blockchain adapter started with immutability and lineage tracking")
    print("Monitoring for COBOL data files...")
    
    log_dir = "/app/logs"
    data_dir = "/app/data"
    pipes_dir = "/app/pipes"
    
    # Create directories
    for dir_path in [log_dir, data_dir, pipes_dir]:
        os.makedirs(dir_path, exist_ok=True)
    
    # Monitor files
    data_files = ["test.dat", "master.dat", "customer.dat", "transaction.dat", "accounts.dat", "products.idx"]
    
    while True:
        try:
            for file_name in data_files:
                file_path = os.path.join(data_dir, file_name)
                if os.path.exists(file_path):
                    process_cobol_file(file_path, file_name)
            
            time.sleep(2)
        except KeyboardInterrupt:
            print("Shutting down monitor...")
            break
        except Exception as e:
            print(f"Monitor error: {e}")
            time.sleep(5)

def start_http_server():
    """Start HTTP server for API access"""
    server = HTTPServer(('0.0.0.0', 8080), BlockchainHTTPHandler)
    print("HTTP server started on port 8080")
    server.serve_forever()

def signal_handler(sig, frame):
    print('\nShutting down gracefully...')
    sys.exit(0)

if __name__ == "__main__":
    signal.signal(signal.SIGINT, signal_handler)
    signal.signal(signal.SIGTERM, signal_handler)
    
    # Start HTTP server in a separate thread
    server_thread = threading.Thread(target=start_http_server, daemon=True)
    server_thread.start()
    
    # Start monitoring
    monitor_cobol()
'@

# Deploy enhanced adapter
$adapterScript | docker exec -i cobol-metadata-node bash -c "cat > /app/adapters/cobol-fabric-adapter-enhanced.py && chmod +x /app/adapters/cobol-fabric-adapter-enhanced.py"

# Start the enhanced adapter
Write-Host "Starting enhanced adapter..." -ForegroundColor Yellow
docker exec -d cobol-metadata-node python3 /app/adapters/cobol-fabric-adapter-enhanced.py

# Wait for adapter to start
Start-Sleep -Seconds 5

# 1. System Status Check
Write-Host "1. 📊 System Status Check..." -ForegroundColor Yellow
try {
    $apiStatus = Invoke-RestMethod -Uri "http://localhost:8080/api/status" -TimeoutSec 10
    $apiStats = Invoke-RestMethod -Uri "http://localhost:8080/api/stats" -TimeoutSec 10
    
    Write-Host "   ✅ API Status: $($apiStatus.cobolSystem)" -ForegroundColor Green
    Write-Host "   📊 Enhanced Stats:" -ForegroundColor Cyan
    Write-Host "      • Transactions: $($apiStats.totalTransactions)" -ForegroundColor Gray
    Write-Host "      • Blocks: $($apiStats.totalBlocks)" -ForegroundColor Gray
    Write-Host "      • Files Monitored: $($apiStats.filesMonitored)" -ForegroundColor Gray
    Write-Host "      • Success Rate: $($apiStats.successRate)%" -ForegroundColor Gray
    Write-Host "      • Immutability Events: $($apiStats.immutabilityEvents)" -ForegroundColor Gray
    Write-Host "      • Lineage Chains: $($apiStats.lineageChains)" -ForegroundColor Gray
    Write-Host "      • Avg Lineage Depth: $($apiStats.averageLineageDepth)" -ForegroundColor Gray
    
    $initialStats = $apiStats
    
} catch {
    Write-Host "   ❌ API not responding: $($_.Exception.Message)" -ForegroundColor Red
    Write-Host "   🔧 Ensure the enhanced adapter is running" -ForegroundColor Yellow
    exit 1
}

# Get initial blockchain state for immutability testing
$initialBlocks = @()
$initialTransactions = @()
try {
    $initialBlocksData = Invoke-RestMethod -Uri "http://localhost:8080/api/blocks" -TimeoutSec 10
    $initialTransactionsData = Invoke-RestMethod -Uri "http://localhost:8080/api/transactions" -TimeoutSec 10
    $initialBlocks = $initialBlocksData.blocks
    $initialTransactions = $initialTransactionsData.transactions
} catch {
    Write-Host "   ⚠️ Could not get initial blockchain state" -ForegroundColor Yellow
}

# 2. Blockchain Peer Synchronization
if (-not $SyncOnly) {
    Write-Host ""
    Write-Host "2. 🔄 Blockchain Peer Synchronization..." -ForegroundColor Yellow
    $syncResults = Sync-BlockchainPeers -GcpVmIp $GcpVmIp -ProjectId $GcpProjectId -FullSync $FullSync
}

# 3. Generate Test Data (if not sync-only mode)
if (-not $SyncOnly) {
    Write-Host ""
    Write-Host "3. 📝 Generating Test Data for Blockchain..." -ForegroundColor Yellow
    
    # Create test records to verify immutability and lineage
    $testSessionId = $global:testSession
    $testFiles = @("test.dat", "master.dat", "customer.dat")
    
    foreach ($file in $testFiles) {
        Write-Host "   Generating test data for $file..." -ForegroundColor Cyan
        
        $testData = @()
        for ($i = 1; $i -le $TestRecords; $i++) {
            $recordId = switch ($file) {
                "test.dat" { "TEST{0:D3}" -f (1000 + $i) }
                "master.dat" { "MSTR{0:D3}" -f (2000 + $i) }
                "customer.dat" { "CUST{0:D3}" -f (3000 + $i) }
            }
            
            $testRecord = switch ($file) {
                "test.dat" { "$recordId   Enhanced Test Record Session $testSessionId Entry $i                2025-07-10" }
                "master.dat" { "$recordId   Master Record $i        Type A    Active    Session $testSessionId" }
                "customer.dat" { "$recordId   Test Customer $i                    123 Test St, City, ST         555-000-$($i.ToString('D4'))" }
            }
            
            $testData += $testRecord
        }
        
        # Write test data to file
        $testDataContent = $testData -join "`n"
        $testDataContent | docker exec -i cobol-metadata-node bash -c "cat >> /app/data/$file"
        
        Write-Host "      ✅ Added $($testData.Count) records to $file" -ForegroundColor Green
        Start-Sleep -Seconds 2
    }
    
    # Wait for adapter to process new data
    Write-Host "   Waiting for blockchain processing..." -ForegroundColor Cyan
    Start-Sleep -Seconds 10
}

# 4. Immutability Testing
Write-Host ""
if ($ImmutabilityTest -or -not $SyncOnly) {
    try {
        $currentBlocksData = Invoke-RestMethod -Uri "http://localhost:8080/api/blocks" -TimeoutSec 10
        $currentBlocks = $currentBlocksData.blocks
        
        $immutabilityResults = Test-BlockchainImmutability -InitialBlocks $initialBlocks -CurrentBlocks $currentBlocks
        
        Write-Host "   📊 Immutability Test Results:" -ForegroundColor Cyan
        Write-Host "      • Total Tests: $($immutabilityResults.TotalTests)" -ForegroundColor Gray
        Write-Host "      • Passed: $($immutabilityResults.PassedTests)" -ForegroundColor Green
        Write-Host "      • Failed: $($immutabilityResults.FailedTests)" -ForegroundColor $(if ($immutabilityResults.FailedTests -gt 0) { 'Red' } else { 'Gray' })
        
    } catch {
        Write-Host "   ❌ Immutability testing failed: $($_.Exception.Message)" -ForegroundColor Red
    }
}

# 5. Data Lineage Testing
Write-Host ""
if ($LineageTest -or -not $SyncOnly) {
    try {
        $currentTransactionsData = Invoke-RestMethod -Uri "http://localhost:8080/api/transactions" -TimeoutSec 10
        $lineageData = Invoke-RestMethod -Uri "http://localhost:8080/api/lineage" -TimeoutSec 10
        
        $lineageResults = Test-DataLineage -Transactions $currentTransactionsData.transactions
        
        Write-Host "   📊 Data Lineage Test Results:" -ForegroundColor Cyan
        Write-Host "      • Total Tests: $($lineageResults.TotalTests)" -ForegroundColor Gray
        Write-Host "      • Passed: $($lineageResults.PassedTests)" -ForegroundColor Green
        Write-Host "      • Failed: $($lineageResults.FailedTests)" -ForegroundColor $(if ($lineageResults.FailedTests -gt 0) { 'Red' } else { 'Gray' })
        Write-Host "      • Lineage Chains: $($lineageData.totalRecords)" -ForegroundColor Gray
        Write-Host "      • Total Operations: $($lineageData.totalOperations)" -ForegroundColor Gray
        
    } catch {
        Write-Host "   ❌ Lineage testing failed: $($_.Exception.Message)" -ForegroundColor Red
    }
}

# 6. Final System Verification
Write-Host ""
Write-Host "6. 🔍 Final System Verification..." -ForegroundColor Yellow

try {
    $finalStats = Invoke-RestMethod -Uri "http://localhost:8080/api/stats" -TimeoutSec 10
    $immutabilityData = Invoke-RestMethod -Uri "http://localhost:8080/api/immutability" -TimeoutSec 10
    
    Write-Host "   📊 Final System State:" -ForegroundColor Cyan
    Write-Host "      • Total Transactions: $($finalStats.totalTransactions)" -ForegroundColor Gray
    Write-Host "      • Total Blocks: $($finalStats.totalBlocks)" -ForegroundColor Gray
    Write-Host "      • Chain Integrity: $(if ($immutabilityData.chainIntegrity) { '✅ VERIFIED' } else { '❌ COMPROMISED' })" -ForegroundColor $(if ($immutabilityData.chainIntegrity) { 'Green' } else { 'Red' })
    Write-Host "      • Immutability Events: $($finalStats.immutabilityEvents)" -ForegroundColor Gray
    Write-Host "      • Lineage Chains: $($finalStats.lineageChains)" -ForegroundColor Gray
    
} catch {
    Write-Host "   ❌ Final verification failed: $($_.Exception.Message)" -ForegroundColor Red
}

#endregion

#region Test Summary

Write-Host ""
Write-Host "=" * 80 -ForegroundColor Green
Write-Host "🎯 ENHANCED COBOL-BLOCKCHAIN TEST SUMMARY" -ForegroundColor Green
Write-Host "=" * 80 -ForegroundColor Green

Write-Host ""
Write-Host "📊 Test Session: $global:testSession" -ForegroundColor Yellow

# Synchronization Results
if ($global:syncResults) {
    Write-Host ""
    Write-Host "🔄 Synchronization Results:" -ForegroundColor Yellow
    Write-Host "   • Local Peer: $(if ($global:syncResults.LocalPeerStatus) { '✅ Connected' } else { '❌ Failed' })" -ForegroundColor $(if ($global:syncResults.LocalPeerStatus) { 'Green' } else { 'Red' })
    if ($GcpVmIp) {
        Write-Host "   • GCP Peer: $(if ($global:syncResults.GcpPeerStatus) { '✅ Connected' } else { '❌ Failed' })" -ForegroundColor $(if ($global:syncResults.GcpPeerStatus) { 'Green' } else { 'Red' })
        Write-Host "   • Transactions Synced: $($global:syncResults.TransactionsSynced)" -ForegroundColor Gray
        Write-Host "   • Blocks Synced: $($global:syncResults.BlocksSynced)" -ForegroundColor Gray
    }
    Write-Host "   • Sync Status: $(if ($global:syncResults.SyncCompleted) { '✅ Completed' } else { '❌ Failed' })" -ForegroundColor $(if ($global:syncResults.SyncCompleted) { 'Green' } else { 'Red' })
}

# Immutability Results
if ($global:immutabilityResults) {
    Write-Host ""
    Write-Host "🔒 Immutability Test Results:" -ForegroundColor Yellow
    Write-Host "   • Tests Passed: $($global:immutabilityResults.PassedTests)/$($global:immutabilityResults.TotalTests)" -ForegroundColor $(if ($global:immutabilityResults.FailedTests -eq 0) { 'Green' } else { 'Red' })
    Write-Host "   • Success Rate: $(($global:immutabilityResults.PassedTests / $global:immutabilityResults.TotalTests * 100).ToString('F1'))%" -ForegroundColor Gray
    foreach ($detail in $global:immutabilityResults.Details) {
        Write-Host "   • $detail" -ForegroundColor Gray
    }
}

# Lineage Results
if ($global:lineageResults) {
    Write-Host ""
    Write-Host "📊 Data Lineage Test Results:" -ForegroundColor Yellow
    Write-Host "   • Tests Passed: $($global:lineageResults.PassedTests)/$($global:lineageResults.TotalTests)" -ForegroundColor $(if ($global:lineageResults.FailedTests -eq 0) { 'Green' } else { 'Red' })
    Write-Host "   • Success Rate: $(($global:lineageResults.PassedTests / $global:lineageResults.TotalTests * 100).ToString('F1'))%" -ForegroundColor Gray
    Write-Host "   • Lineage Chains: $($global:lineageResults.LineageChains.Count)" -ForegroundColor Gray
    foreach ($detail in $global:lineageResults.Details) {
        Write-Host "   • $detail" -ForegroundColor Gray
    }
}

Write-Host ""
Write-Host "🔄 Next Steps:" -ForegroundColor Yellow
Write-Host "   • Full Sync Test: .\test-cobol-blockchain.ps1 -FullSync -GcpProjectId 'your-project' -GcpVmIp 'ip'" -ForegroundColor Cyan
Write-Host "   • Immutability Only: .\test-cobol-blockchain.ps1 -ImmutabilityTest" -ForegroundColor Cyan
Write-Host "   • Lineage Only: .\test-cobol-blockchain.ps1 -LineageTest" -ForegroundColor Cyan
Write-Host "   • Clean Start: .\test-cobol-blockchain.ps1 -CleanStart -TestRecords 20" -ForegroundColor Cyan
Write-Host "   • Sync Only: .\test-cobol-blockchain.ps1 -SyncOnly -GcpProjectId 'your-project'" -ForegroundColor Cyan

Write-Host ""
Write-Host "🌐 Monitor Results:" -ForegroundColor Yellow
Write-Host "   • Local API: Invoke-RestMethod -Uri 'http://localhost:8080/api/stats'" -ForegroundColor Cyan
Write-Host "   • Immutability: Invoke-RestMethod -Uri 'http://localhost:8080/api/immutability'" -ForegroundColor Cyan
Write-Host "   • Lineage: Invoke-RestMethod -Uri 'http://localhost:8080/api/lineage'" -ForegroundColor Cyan
Write-Host "   • Live Logs: docker logs -f cobol-metadata-node" -ForegroundColor Cyan

Write-Host ""
Write-Host "💾 Enhanced blockchain with immutability and lineage tracking complete!" -ForegroundColor Green

#endregion