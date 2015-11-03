#!/bin/bash

echo "$1"
echo "$2"
echo "----------"
echo "">temp.txt

cat $2 | while read line;do
    echo $1$line>>temp.txt
done

mv temp.txt $2

