 #!/bin/bash
rm -rf cache/downloaded.png
date1=$(date +"%s")
wget -O cache/downloaded.png --input-file=$1  -U DVScache
date2=$(date +"%s")
diff=$(($date2-$date1))
echo "---------------------------"
echo "---------------------------"
echo "---------------------------"
echo "$(($diff / 60)) minutes and $(($diff % 60)) seconds elapsed."
echo "---------------------------"
ls -all cache/downloaded.png
rm -rf cache/downloaded.png
echo "---------------------------"
