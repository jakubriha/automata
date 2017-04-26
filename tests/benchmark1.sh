#!/bin/bash

if [ "$1" == "vata" ]; then
  program="vata -t -r expl_fa incl "
elif [ "$1" == "automata" ]; then
  program="automata-exe universality"  #######
else
  echo "Program not supported."
  exit 1
fi

directory="/home/jakub/Desktop/fas-univ"  #######
csv="/home/jakub/Sources/thesis/data/univ-antichain-set-automata.csv"  #######

echo "x,y,z" > $csv

files=$(find $directory -type f | sort)

for file in $files
do
 
  if [ "$1" == "vata" ]; then
    # Execute program and save stderr to time.
    $program $file >> /tmp/results-automata 2> /tmp/error
    time=$(</tmp/error)

    # Get rid of scientific notation
    time=`echo ${time} | sed -e 's/[eE]+*/\\*10\\^/'`

    # Time in seconds convert to miliseconds
    time=$(echo "1000 * $time" | bc -l)

    states=$(basename "$file")
    states="${states%.*}"
  else
    # Execute program and save output.
    result=$($program $file)

    result=$(echo $result | grep "time" | grep -o "time [0-9]*\.[0-9]* [^ 0-9]*")
    time=$(echo $result | grep -o "[0-9]*\.[0-9]*")
    unit=$(echo $result | grep -o "[^ 0-9]s")
    multiply=""

    if [ "$unit" == "ms" ]; then
      multiply="1"
    elif [ "$unit" == "μs" ]; then
      multiply="0.001"
    elif [ "$unit" == "ns" ]; then
      multiply="0.000001"
    else
      echo "unknown unit" $unit
      exit
    fi

    # Convert time to miliseconds
    time=$(echo "$multiply" " * $time" | bc -l)

    states=$(basename "$file")
    states="${states%.*}"
  fi

  printf "%s,%s\n" "$states" "$time" >> $csv
  printf "%s,%s\n" "$states" "$time"

done
