#!/bin/bash

# Initialize environment (now we're in /app inside container)
echo "Starting COBOL-Blockchain Metadata Services..."

# Create necessary directories
mkdir -p data logs pipes /tmp

# Create named pipes
./scripts/create-pipes.sh

# Since we're in /app, adjust paths
echo "Starting services from $(pwd)..."

# Start monitoring (placeholder for now)
echo "COBOL-Blockchain container is running..."
echo "Data directory: $(pwd)/data"
echo "Logs directory: $(pwd)/logs"

# Keep container running
tail -f /dev/null