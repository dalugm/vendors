#!/usr/bin/env bash

read -p "Input the index you want to start: " index
read -p "Input file format: " suffix

for file in $(ls *.$suffix)
do
    mv $file $index.$suffix
    index=$((index+1))
done
