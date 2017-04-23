#!/bin/bash

if [ "$1" == "vata" ]; then
  program="vata -t -r expl_fa isect " #######
elif [ "$1" == "automata" ]; then
  program="automata-exe universality"
else
  echo "Program not supported."
  exit 1
fi

directory="/home/jakub/Desktop/fas"
csv="/home/jakub/Sources/thesis/data/isect-vata.csv"  #######

echo "x,y,z" > $csv

files=$(find $directory -type f | sort)

for file1 in $files
do

  for file2 in $files
  do
 
    if [ "$1" == "vata" ]; then
      # Execute program and save stderr to time.
      $program $file1 $file2 >> /tmp/results-automata 2> /tmp/error
      time=$(</tmp/error)

      # Get rid of scientific notation
      time=`echo ${time} | sed -e 's/[eE]+*/\\*10\\^/'`

      # Time in seconds convert to miliseconds
      time=$(echo "1000 * $time" | bc -l)

      states1=$(basename "$file1")
      states1="${states1%.*}"
      states2=$(basename "$file2")
      states2="${states2%.*}"
    else
      # Execute program and save output.
      result=$($program $file1 $file2)

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

      states1=$(basename "$file1")
      states1="${states1%.*}"
      states2=$(basename "$file2")
      states2="${states2%.*}"
    fi

    printf "%s,%s,%s\n" "$states1" "$states2" "$time" >> $csv
    printf "%s,%s,%s\n" "$states1" "$states2" "$time"

    sleep .5

  done

  printf "\n" >> $csv

done
