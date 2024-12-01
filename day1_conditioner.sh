#!/bin/bash
file=$1
echo Conditioning $file
awk '{print $1|"sort > sleft.txt"; print $2|"sort > sright.txt";}' $file
paste sleft.txt sright.txt > ${file%.txt}_conditioned.txt
rm sleft.txt sright.txt
