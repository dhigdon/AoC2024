#!/bin/bash
file=$1
echo Conditioning $file
rm -f left.txt right.txt
awk '{print $1>"left.txt"; print $2>"right.txt";}' $file
sort left.txt > sleft.txt
sort right.txt > sright.txt
paste sleft.txt sright.txt > ${file%.txt}_conditioned.txt
rm left.txt sleft.txt right.txt sright.txt
