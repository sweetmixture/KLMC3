#!/bin/bash

sed -i "s/0  1  0  1  1/0 1 0 1 1 1/g"  `grep '0  1  0  1  1 ' -rl *.gin`
sed -i '/output xyz/d' *.gin
rm -r *.out
