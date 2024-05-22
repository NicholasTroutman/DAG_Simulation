#!/bin/bash

BRed='\033[1;31m'         # Red
BGreen='\033[1;32m'       # Green
BYellow='\033[1;33m'      # Yellow
BBlue='\033[1;34m'        # Blue
NC='\033[0m' # No Color


printf "RSUs"

for rsus in  1 2 3 4 5 6 7 8
do
	printf "\n\n ${BRed}mtxs ~ $mtxs${NC}\n\n"
	
	for volume in  1
	do
		printf "\n\n ${BGreen}volume ~ $volume${NC}\n\n"
		
		for netsize in   100
		do
			python3 core.py --txs 10000 --netsize $netsize --printing False --dltmode dht --consensus individual --map "HoustonHwyredblue"  --seed 1  --ref 1 --volume $volume --rsu $rsus --pruning 0 --maxTxs 8888 --minTxs 30 --blocktime 40 --group 2
		done
	done
done