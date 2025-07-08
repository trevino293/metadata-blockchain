#!/bin/bash

# Create named pipes for COBOL-Blockchain communication
PIPE_DIR="/tmp"

pipes=(
    "cobol-triggers.pipe"
    "blockchain-response.pipe"
    "batch-metadata.pipe"
)

for pipe in "${pipes[@]}"; do
    if [ ! -p "$PIPE_DIR/$pipe" ]; then
        mkfifo "$PIPE_DIR/$pipe"
        echo "Created pipe: $PIPE_DIR/$pipe"
    else
        echo "Pipe already exists: $PIPE_DIR/$pipe"
    fi
done

# Set permissions
chmod 666 $PIPE_DIR/*.pipe 2>/dev/null || true