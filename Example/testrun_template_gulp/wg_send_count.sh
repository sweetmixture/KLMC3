#!/bin/bash

max_wg=128
for ((i=0;i<${max_wg};i++)); do

	cnt1=$(grep "task result send" workgroup_${i}.log  | wc -l)
	cnt2=$(grep "task recv" workgroup_${i}.log  | wc -l)
	printf "%3d%10d%10d\n" ${i} ${cnt1} ${cnt2}
done
