#!/usr/bin/env bash

format="User-mode CPU seconds: %U, percent CPU: %P"
ntimes=1000
for i in tests/*.pd.bin; do
    echo "Test: $i"
    echo "    reference implementation... $(time -f "$format"  -o /dev/stderr sh -c "seq $ntimes | xargs -Iz tests/$(basename $i .pd.bin).bin" 2>&1 >/dev/null) "
    echo "    LLVM... $(time -f "$format" -o /dev/stderr sh -c "seq $ntimes | xargs -Iz tests/$(basename $i .pd.bin).pd.bin" 2>&1 >/dev/null) "
done
