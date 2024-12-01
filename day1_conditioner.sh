#!/bin/bash
echo Conditioning $1
rm -f left.txt right.txt
awk '{print $1>"left.txt"; print $2>"right.txt";}' $1
sort left.txt > sleft.txt
sort right.txt > sright.txt
paste sleft.txt sright.txt > day1_conditioned.txt
rm left.txt sleft.txt right.txt sright.txt
