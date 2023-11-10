#!/bin/bash

sta=100
end=144

for ((i=$sta; i<=$end; i++)); do
    stt=$(grep "Job Started" ./A${i}/*gout)
    en=$(grep "Job Finished" ./A${i}/*gout)
    printf "%3d%64.48s%64.48s\n" ${i} "${stt}" "${en}"
done
