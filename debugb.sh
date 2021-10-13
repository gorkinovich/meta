#!/bin/bash
mkdir -p out/production/meta
for file in source/*.erl; do
    echo ">>> Compiling file: $file"
    erlc +debug_info -o out/production/meta $file
done
