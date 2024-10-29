max=$1

for(( i=0; i<=${max}; i++)); do

	mv X${i}.gin A${i}.gin
	# sed -i "s|Tc|Mn|" A${i}.gin
	sed -i "s|output xyz|#output xyz|" A${i}.gin

	#echo ${i}
done
