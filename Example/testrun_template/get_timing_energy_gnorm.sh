#!/bin/bash

sta=0
end=99

for ((i=$sta; i<=$end; i++)); do
    stt=$(grep "Job Started" ./A${i}/*gout)
    en=$(grep "Job Finished" ./A${i}/*gout)

	# Final energy =
	# Final Gnorm  =
	energy=$(grep "Final energy = " ./A${i}/*gout | awk '{print $4}')
	gnorm=$(grep "Final Gnorm = " ./A${i}/*gout | awk '{print $4}')
    printf "%3d%64.48s%64.48s%16.9lf%16.9lf\n" ${i} "${stt}" "${en}" "${energy}" "${gnorm}"
done
