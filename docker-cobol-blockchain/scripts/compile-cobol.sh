#!/bin/bash
# scripts/compile-cobol.sh - Compile all COBOL programs

echo "Compiling COBOL programs..."

# Create bin directory for compiled programs
mkdir -p bin

# Compile each COBOL program
for cob_file in cobol_programs/*.cob; do
    if [ -f "$cob_file" ]; then
        filename=$(basename "$cob_file" .cob)
        echo "Compiling $filename..."
        cobc -x -o "bin/$filename" "$cob_file"
        if [ $? -eq 0 ]; then
            echo "  ✓ $filename compiled successfully"
        else
            echo "  ✗ Error compiling $filename"
        fi
    fi
done

echo "Compilation complete!"
ls -la bin/