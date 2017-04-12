#!/bin/bash

if [ "$1" == "vata" ]; then
  program="vata -t -r expl_fa incl "
elif [ "$1" == "automata" ]; then
  program="automata-exe "
else
  echo "Program not supported."
  exit 1
fi

directory="/home/jakub/Sources/automata-benchmarks"
faCount=$2

tempFile="$(mktemp)"

# Get *.tmb filepaths that will be used in benchmark and save them to $temFile.
find "$directory" -name "*.tmb" | head -"$faCount" > "$tempFile"

# Convert $tempFile content to array.
IFS=$'\r\n' GLOBIGNORE='*' command eval  'filepaths=($(cat "$tempFile"))'

> "$tempFile"

# For each pair of filepaths...
for i in "${!filepaths[@]}"; do 
  for j in "${!filepaths[@]}"; do 

    # Execute program and pipe stderr to $tempFile.
    $program ${filepaths[$i]} ${filepaths[$j]} 2>>"$tempFile" >/dev/null

  done
done

# Get average of values in $tempFile.
awk '{ total += $1 } END { print total/NR }' "$tempFile"
