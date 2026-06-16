#!/bin/sh
# Create lea.exe with embedded help/samples ZIP appended.
# Run this after gprbuild produces lea_without_data.exe(.so).
set -e
cd "$(dirname "$0")"
cp lea_without_data.exe.so lea.exe.so
cat _lea_data.zip >> lea.exe.so
sed 's/lea_without_data\.exe\.so/lea.exe.so/' lea_without_data.exe > lea.exe
chmod +x lea.exe
