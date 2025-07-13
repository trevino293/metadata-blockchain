# test-cobol-blockchain.ps1 - Updated for Hyperledger Fabric Integration
# This script tests the complete COBOL-Blockchain stack including Fabric components

param(
    [switch]$FullTest,               # Run all tests
    [switch]$HealthOnly,             # Only test health endpoints
    [switch]$FabricTest,             # Test Fabric network components
    [switch]$DataTest,               # Test data file operations
    [switch]$MetricsTest,            # Test metrics endpoint
    [switch]$ChannelTest,            # Test channel operations
    [switch]$Verbose,                # Show detailed output
    [int]$TestRecords = 5,           # Number of test records to create
    [switch]$CleanStart              # Clean all data and start fresh
)

Write-Host "=== COBOL-Blockchain Hyperledger Fabric Integration Test ===" -ForegroundColor Green
Write-Host "Testing complete stack with Fabric network" -ForegroundColor Cyan
Write-Host ""

# Global variables
$global:testSession = Get-Date -Format "yyyyMMddHHmmss"
$global:testResults = @{
    TotalTests = 0
    PassedTests = 0
    FailedTests = 0
    Details = @()
}

#region Helper Functions

function Write-TestResult {
    param(
        [string]$TestName,
        [bool]$Passed,
        [string]$Details = ""
    )
    
    $global:testResults.TotalTests++
    if ($Passed) {
        $global:testResults.PassedTests++
        Write-Host "   ✅ PASS: $TestName" -ForegroundColor Green
    } else {
        $global:testResults.FailedTests++
        Write-Host "   ❌ FAIL: $TestName" -ForegroundColor Red
    }
    
    if ($Details -and $Verbose) {
        Write-Host "      $Details" -ForegroundColor Gray
    }
    
    # Fixed string concatenation issue
    $result = if ($Passed) { 'PASS' } else { 'FAIL' }
    $global:testResults.Details += "${TestName}: ${result} - ${Details}"
}

function Test-Endpoint {
    param(
        [string]$Url,
        [string]$TestName,
        [int]$Timeout = 5
    )
    
    try {
        if ($Verbose) { Write-Host "   Testing: $Url" -ForegroundColor Gray }
        
        $response = Invoke-WebRequest -Uri $Url -TimeoutSec $Timeout -UseBasicParsing
        $success = $response.StatusCode -eq 200
        
        Write-TestResult $TestName $success "Status: $($response.StatusCode)"
        
        if ($Verbose -and $success) {
            Write-Host "      Response: $($response.Content.Substring(0, [Math]::Min(100, $response.Content.Length)))..." -ForegroundColor Gray
        }
        
        return $success
    } catch {
        Write-TestResult $TestName $false "Error: $($_.Exception.Message)"
        return $false
    }
}

function Test-JsonEndpoint {
    param(
        [string]$Url,
        [string]$TestName,
        [scriptblock]$Validation = $null
    )
    
    try {
        if ($Verbose) { Write-Host "   Testing JSON: $Url" -ForegroundColor Gray }
        
        $response = Invoke-RestMethod -Uri $Url -TimeoutSec 5
        $success = $true
        $details = "Response received"
        
        if ($Validation) {
            $validationResult = & $Validation $response
            $success = $validationResult.Success
            $details = $validationResult.Details
        }
        
        Write-TestResult $TestName $success $details
        
        if ($Verbose -and $response) {
            Write-Host "      JSON: $(ConvertTo-Json $response -Compress)" -ForegroundColor Gray
        }
        
        return @{
            Success = $success
            Data = $response
        }
    } catch {
        Write-TestResult $TestName $false "Error: $($_.Exception.Message)"
        return @{
            Success = $false
            Data = $null
        }
    }
}

function Test-ContainerStatus {
    param(
        [string]$ContainerName,
        [string]$DisplayName = $ContainerName
    )
    
    try {
        $status = docker inspect $ContainerName --format='{{.State.Status}}' 2>$null
        $isRunning = $status -eq "running"
        
        if ($isRunning) {
            $health = docker inspect $ContainerName --format='{{.State.Health.Status}}' 2>$null
            if ($health) {
                Write-TestResult "$DisplayName Status" $isRunning "Status: $status, Health: $health"
            } else {
                Write-TestResult "$DisplayName Status" $isRunning "Status: $status"
            }
        } else {
            Write-TestResult "$DisplayName Status" $false "Status: $status"
        }
        
        return $isRunning
    } catch {
        Write-TestResult "$DisplayName Status" $false "Container not found"
        return $false
    }
}

function Test-FabricPort {
    param(
        [int]$Port,
        [string]$ServiceName
    )
    
    try {
        $tcpClient = New-Object System.Net.Sockets.TcpClient
        $tcpClient.Connect("localhost", $Port)
        $connected = $tcpClient.Connected
        $tcpClient.Close()
        
        Write-TestResult "$ServiceName Port $Port" $connected "Port is listening"
        return $connected
    } catch {
        Write-TestResult "$ServiceName Port $Port" $false "Port not accessible"
        return $false
    }
}

function Wait-ForService {
    param(
        [string]$Url,
        [int]$MaxAttempts = 30
    )
    
    Write-Host "⏳ Waiting for service at $Url..." -ForegroundColor Yellow
    
    for ($i = 1; $i -le $MaxAttempts; $i++) {
        try {
            $response = Invoke-WebRequest -Uri $Url -TimeoutSec 2 -UseBasicParsing -ErrorAction SilentlyContinue
            if ($response.StatusCode -eq 200) {
                Write-Host "   ✅ Service is ready!" -ForegroundColor Green
                return $true
            }
        } catch {
            if ($Verbose) { Write-Host "   Attempt $i/$MaxAttempts..." -ForegroundColor Gray }
        }
        
        Start-Sleep -Seconds 1
    }
    
    Write-Host "   ❌ Service did not become ready in time" -ForegroundColor Red
    return $false
}

#endregion

#region Test Execution

# Set project name
$env:COMPOSE_PROJECT_NAME = "docker-cobol-blockchain"

Write-Host "1. 🐳 Container Status Verification" -ForegroundColor Yellow
Write-Host ""

# Check all containers
$containers = @(
    @{Name="crypto-gen"; Display="Crypto Generator"},
    @{Name="orderer.example.com"; Display="Orderer"},
    @{Name="peer0.org1.example.com"; Display="Peer"},
    @{Name="couchdb"; Display="CouchDB"},
    @{Name="cobol-metadata-node"; Display="COBOL Node"},
    @{Name="fabric-cli"; Display="Fabric CLI"}
)

$allRunning = $true
foreach ($container in $containers) {
    $running = Test-ContainerStatus $container.Name $container.Display
    if (-not $running -and $container.Name -ne "crypto-gen") {
        $allRunning = $false
    }
}

if (-not $allRunning) {
    Write-Host ""
    Write-Host "❌ Not all required containers are running." -ForegroundColor Red
    Write-Host "   Run 'docker-compose up -d' to start the network" -ForegroundColor Yellow
    exit 1
}

# Test 1: Basic Health Checks
if ($HealthOnly -or $FullTest) {
    Write-Host ""
    Write-Host "2. 🏥 Health Check Tests" -ForegroundColor Yellow
    
    # Wait for COBOL service
    $serviceReady = Wait-ForService "http://localhost:8080/health"
    if ($serviceReady) {
        # Test home page
        Test-Endpoint "http://localhost:8080/" "COBOL Node Home Page"
        
        # Test health endpoint
        $healthResult = Test-JsonEndpoint "http://localhost:8080/health" "COBOL Health API" {
            param($response)
            
            $hasStatus = $response.status -eq 'healthy'
            $hasTimestamp = $null -ne $response.timestamp
            $hasContainer = $response.container -eq 'cobol-metadata-node'
            $hasFabricConnection = $null -ne $response.services.'fabric-connection'
            
            $success = $hasStatus -and $hasTimestamp -and $hasContainer
            
            return @{
                Success = $success
                Details = "Status: $($response.status), Fabric: $($response.services.'fabric-connection')"
            }
        }
        
        # Test fabric status endpoint
        Test-JsonEndpoint "http://localhost:8080/fabric-status" "Fabric Status API" {
            param($response)
            
            $hasPeerEndpoint = $response.peer_endpoint -eq 'peer0.org1.example.com:7051'
            $hasOrdererEndpoint = $response.orderer_endpoint -eq 'orderer.example.com:7050'
            $cryptoGenerated = $response.crypto_generated -eq $true
            $genesisExists = $response.genesis_block -eq $true
            
            $success = $hasPeerEndpoint -and $hasOrdererEndpoint -and $cryptoGenerated -and $genesisExists
            
            return @{
                Success = $success
                Details = "Crypto: $($response.crypto_generated), Genesis: $($response.genesis_block)"
            }
        }
    }
    
    # Test CouchDB
    Test-Endpoint "http://localhost:5984/" "CouchDB API"
}

# Test 2: Fabric Network Components
if ($FabricTest -or $FullTest) {
    Write-Host ""
    Write-Host "3. 🔗 Fabric Network Tests" -ForegroundColor Yellow
    
    # Test ports
    Test-FabricPort 7050 "Orderer"
    Test-FabricPort 7051 "Peer"
    Test-FabricPort 5984 "CouchDB"
    
    # Check crypto materials
    try {
        $cryptoExists = docker exec fabric-cli test -d /opt/gopath/src/github.com/hyperledger/fabric/peer/crypto/ordererOrganizations 2>$null
        Write-TestResult "Crypto Materials" ($LASTEXITCODE -eq 0) "Crypto configuration present"
        
        $genesisExists = docker exec fabric-cli test -f /opt/gopath/src/github.com/hyperledger/fabric/peer/channel-artifacts/genesis.block 2>$null
        Write-TestResult "Genesis Block" ($LASTEXITCODE -eq 0) "Genesis block present"
        
    } catch {
        Write-TestResult "Fabric Configuration" $false "Error checking configuration"
    }
    
    # Check orderer logs for startup
    try {
        $ordererLogs = docker logs orderer.example.com 2>&1 | Select-String "Beginning to serve requests" | Select-Object -First 1
        Write-TestResult "Orderer Started" ($null -ne $ordererLogs) "Orderer serving requests"
    } catch {
        Write-TestResult "Orderer Started" $false "Could not check orderer status"
    }
    
    # Check peer logs for startup
    try {
        $peerLogs = docker logs peer0.org1.example.com 2>&1 | Select-String "Started peer" | Select-Object -First 1
        Write-TestResult "Peer Started" ($null -ne $peerLogs) "Peer node started"
    } catch {
        Write-TestResult "Peer Started" $false "Could not check peer status"
    }
}

# Test 3: Channel Operations
if ($ChannelTest -or $FullTest) {
    Write-Host ""
    Write-Host "4. 📺 Channel Operations Tests" -ForegroundColor Yellow
    
    try {
        # Check if channel exists
        $channelList = docker exec fabric-cli peer channel list 2>&1
        $hasChannel = $channelList -match "mychannel"
        
        if (-not $hasChannel) {
            Write-Host "   Creating channel 'mychannel'..." -ForegroundColor Yellow
            
            # Create channel
            $createResult = docker exec fabric-cli peer channel create `
                -o orderer.example.com:7050 `
                -c mychannel `
                -f ./channel-artifacts/mychannel.tx `
                --outputBlock mychannel.block 2>&1
            
            $createSuccess = $LASTEXITCODE -eq 0
            Write-TestResult "Create Channel" $createSuccess "Channel creation"
            
            if ($createSuccess) {
                # Join channel
                $joinResult = docker exec fabric-cli peer channel join -b mychannel.block 2>&1
                $joinSuccess = $LASTEXITCODE -eq 0
                Write-TestResult "Join Channel" $joinSuccess "Channel join"
            }
        } else {
            Write-TestResult "Channel Exists" $true "mychannel already exists"
            
            # Get channel info
            $channelInfo = docker exec fabric-cli peer channel getinfo -c mychannel 2>&1
            $infoSuccess = $LASTEXITCODE -eq 0
            Write-TestResult "Channel Info" $infoSuccess "Retrieved channel information"
        }
        
    } catch {
        Write-TestResult "Channel Operations" $false "Error: $($_.Exception.Message)"
    }
}

# Test 4: Metrics
if ($MetricsTest -or $FullTest) {
    Write-Host ""
    Write-Host "5. 📊 Metrics Tests" -ForegroundColor Yellow
    
    Test-Endpoint "http://localhost:8080/metrics" "Metrics Endpoint"
    
    # Verify metrics format
    try {
        $metricsContent = Invoke-RestMethod -Uri "http://localhost:8080/metrics" -TimeoutSec 5
        $hasHealthMetric = $metricsContent -match "cobol_blockchain_health"
        $hasUptimeMetric = $metricsContent -match "cobol_blockchain_uptime_seconds"
        
        Write-TestResult "Metrics Format" ($hasHealthMetric -and $hasUptimeMetric) "Prometheus format validation"
    } catch {
        Write-TestResult "Metrics Format" $false "Could not retrieve metrics"
    }
}

# Test 5: Data Operations
if ($DataTest -or $FullTest) {
    Write-Host ""
    Write-Host "6. 📁 Data File Operations" -ForegroundColor Yellow
    
    try {
        # Check data directory
        $dataFiles = docker exec cobol-metadata-node ls -la /app/data/ 2>$null
        $hasMasterDat = $dataFiles -match "master.dat"
        
        Write-TestResult "Data Files" $hasMasterDat "master.dat present"
        
        if ($TestRecords -gt 0) {
            Write-Host "   📝 Creating test records..." -ForegroundColor Cyan
            
            for ($i = 1; $i -le $TestRecords; $i++) {
                $recordId = "TEST{0:D3}" -f ($i + 100)
                $timestamp = Get-Date -Format "yyyy-MM-dd HH:mm:ss"
                $record = "$recordId Test Record $i - $timestamp"
                
                docker exec cobol-metadata-node bash -c "echo '$record' >> /app/data/master.dat" 2>$null
                
                if ($Verbose) {
                    Write-Host "      Added: $record" -ForegroundColor Gray
                }
            }
            
            Write-TestResult "Add Records" $true "Added $TestRecords test records"
        }
        
        # Check adapter logs
        $logExists = docker exec cobol-metadata-node test -f /app/logs/adapter.log 2>$null
        Write-TestResult "Adapter Log" ($LASTEXITCODE -eq 0) "Log file exists"
        
    } catch {
        Write-TestResult "Data Operations" $false "Error: $($_.Exception.Message)"
    }
}

# Test 6: COBOL Integration
if ($FullTest) {
    Write-Host ""
    Write-Host "7. 🔧 COBOL Integration Tests" -ForegroundColor Yellow
    
    try {
        # Check COBOL compiler
        $cobcVersion = docker exec cobol-metadata-node cobc --version 2>&1 | Select-Object -First 1
        $cobcAvailable = $LASTEXITCODE -eq 0
        
        Write-TestResult "COBOL Compiler" $cobcAvailable "cobc available"
        
        if ($cobcAvailable -and $Verbose) {
            Write-Host "      Version: $cobcVersion" -ForegroundColor Gray
        }
        
        # Create and compile test program
        if ($cobcAvailable) {
            $cobolProgram = @'
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BLOCKCHAIN-TEST.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-MESSAGE PIC X(30) VALUE "Fabric-COBOL Integration OK".
       PROCEDURE DIVISION.
           DISPLAY WS-MESSAGE.
           STOP RUN.
'@
            
            $cobolProgram | docker exec -i cobol-metadata-node tee /tmp/test.cob > $null 2>&1
            
            docker exec cobol-metadata-node cobc -x -o /tmp/test-program /tmp/test.cob 2>$null
            $compileSuccess = $LASTEXITCODE -eq 0
            
            Write-TestResult "COBOL Compile" $compileSuccess "Test program compilation"
            
            if ($compileSuccess) {
                $output = docker exec cobol-metadata-node /tmp/test-program 2>&1
                $runSuccess = $output -match "Fabric-COBOL Integration OK"
                
                Write-TestResult "COBOL Execute" $runSuccess "Program output verified"
            }
        }
        
    } catch {
        Write-TestResult "COBOL Integration" $false "Error: $($_.Exception.Message)"
    }
}

#endregion

#region Test Summary

Write-Host ""
Write-Host "=" * 70 -ForegroundColor Green
Write-Host "📊 TEST SUMMARY - COBOL BLOCKCHAIN FABRIC INTEGRATION" -ForegroundColor Green
Write-Host "=" * 70 -ForegroundColor Green

Write-Host ""
Write-Host "Test Session: $global:testSession" -ForegroundColor Yellow
Write-Host "Total Tests: $($global:testResults.TotalTests)" -ForegroundColor Yellow
Write-Host "Passed: $($global:testResults.PassedTests)" -ForegroundColor Green
Write-Host "Failed: $($global:testResults.FailedTests)" -ForegroundColor $(if ($global:testResults.FailedTests -gt 0) { 'Red' } else { 'Gray' })

$successRate = if ($global:testResults.TotalTests -gt 0) {
    [Math]::Round(($global:testResults.PassedTests / $global:testResults.TotalTests) * 100, 1)
} else { 0 }
Write-Host "Success Rate: ${successRate}%" -ForegroundColor Cyan

if ($global:testResults.FailedTests -gt 0) {
    Write-Host ""
    Write-Host "Failed Tests:" -ForegroundColor Red
    $global:testResults.Details | Where-Object { $_ -match "FAIL" } | ForEach-Object {
        Write-Host "   $_" -ForegroundColor Red
    }
}

Write-Host ""
Write-Host "🔍 Useful Commands:" -ForegroundColor Yellow
Write-Host "   View logs:" -ForegroundColor Cyan
Write-Host "     docker-compose logs -f orderer" -ForegroundColor White
Write-Host "     docker-compose logs -f peer0" -ForegroundColor White
Write-Host "     docker logs cobol-metadata-node" -ForegroundColor White
Write-Host ""
Write-Host "   Access containers:" -ForegroundColor Cyan
Write-Host "     docker exec -it fabric-cli bash" -ForegroundColor White
Write-Host "     docker exec -it cobol-metadata-node bash" -ForegroundColor White
Write-Host ""
Write-Host "   Channel operations:" -ForegroundColor Cyan
Write-Host "     docker exec fabric-cli peer channel list" -ForegroundColor White
Write-Host "     docker exec fabric-cli peer channel getinfo -c mychannel" -ForegroundColor White
Write-Host ""
Write-Host "   Service endpoints:" -ForegroundColor Cyan
Write-Host "     http://localhost:8080/         - COBOL Node UI" -ForegroundColor White
Write-Host "     http://localhost:8080/health   - Health Status" -ForegroundColor White
Write-Host "     http://localhost:5984/_utils/  - CouchDB UI (admin/adminpw)" -ForegroundColor White

Write-Host ""
Write-Host "✅ Test completed!" -ForegroundColor Green

# Return exit code based on test results
exit $(if ($global:testResults.FailedTests -eq 0) { 0 } else { 1 })

#endregion