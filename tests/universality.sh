#!/bin/bash

stateCount=400

directory="/home/jakub/Desktop/fas-univ"

rm -fr $directory

for i in $(seq 1 $stateCount)
do
  file="$directory/$i.tmb"

  mkdir -p $directory && touch $i.tmb

  states=()

  for j in $(seq 1 $i)
  do
    states+=("$j")
  done

  printf "Ops a:1 x:0\n\n" >> $file
  printf "Automaton A" >> $file
  
  printf "\nStates " >> $file
  for state in "${states[@]}"
  do
    printf "%s " "$state" >> $file
  done

  printf "\nFinal States " >> $file
  for state in "${states[@]}"
  do
    printf "%s " "$state" >> $file
  done

  unset 'states[${#states[@]}-1]'

  printf "\nTransitions\nx -> 1\n" >> $file
  for state in "${states[@]}"
  do
    nextState=$((state+1))
    
    printf "a(%s) -> %s\n" "$state" "$nextState" >> $file
  done
done