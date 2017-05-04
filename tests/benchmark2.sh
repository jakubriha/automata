#!/bin/bash 
 
if [ "$1" == "vata" ]; then 
  program="vata -t -r expl_fa incl " 
elif [ "$1" == "automata" ]; then 
  program="automata-exe " 
else 
  echo "Program not supported." 
  exit 1 
fi 
 
directory="/home/jakub/Desktop/fas" 
csv="/home/jakub/Sources/thesis/data/performance-plots/union-automata-set.csv" 

echo $csv 

echo "x,y" > $csv 
 
files=$(find $directory -type f | sort) 
 
for file1 in $files 
do 
  for file2 in $files 
  do 
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
 
    printf "%s,%s,%s\n" "$states1" "$states2" "$time" >> $csv 
    printf "%s,%s,%s\n" "$states1" "$states2" "$time"
  done 
 
done 