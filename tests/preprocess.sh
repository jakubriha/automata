#!/bin/bash

directory="/home/jakub/Sources/automata-benchmarks"
output="/home/jakub/Desktop/fas"

# Get *.tmb filepaths that will be preprocessed.
files=$(find $directory -name "*.tmb")

for file in $files
do
    states=$(grep -m 1 States $file | wc -w | sed -e :a -e 's/^.\{1,3\}$/0&/;ta')

    cp $file $output/$states.tmb
done
