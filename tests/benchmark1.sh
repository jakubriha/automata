#!/bin/bash 
 
if [ "$1" == "vata" ]; then 
  program="vata -t -r expl_fa incl " 
elif [ "$1" == "automata" ]; then 
  program="automata-exe " 
else 
  echo "Program not supported." 
  exit 1 
fi 
 
directory="/home/jakub/Desktop/fas-univ" 
csv="/home/jakub/Sources/thesis/data/performance-plots/univ-automata-set.csv" 
 
echo "x,y,z" > $csv 
 
files=$(find $directory -type f | sort) 
 
for file in $files 

  do 
    # Execute program and save stderr to time. 
    $program $file >> /tmp/results-automata 2> /tmp/error 
    time=$(</tmp/error) 
 
    # Get rid of scientific notation 
    time=`echo ${time} | sed -e 's/[eE]+*/\\*10\\^/'` 
 
    # Time in seconds convert to miliseconds 
    time=$(echo "1000 * $time" | bc -l) 
 
    states=$(basename "$file") 
    states="${states%.*}" 
 
    printf "%s,%s\n" "$states" "$time" >> $csv 
    printf "%s,%s\n" "$states" "$time"
   
  done 
