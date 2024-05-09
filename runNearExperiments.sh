#!/bin/bash

BRed='\033[1;31m'         # Red
BGreen='\033[1;32m'       # Green
BYellow='\033[1;33m'      # Yellow
BBlue='\033[1;34m'        # Blue
NC='\033[0m' # No Color


printf "NAIVE Linear PoW"

for mtxs in  0 5 10 15
do
	printf "\n\n ${BRed}mtxs ~ $mtxs${NC}\n\n"
	
	for volume in  1
	do
		printf "\n\n ${BGreen}volume ~ $volume${NC}\n\n"
		
		for netsize in   100
		do
			python3 core.py --txs 10000 --netsize $netsize --printing False --dltmode linear --consensus individual --map "HoustonHwyredblue"  --seed 1  --ref 2 --volume $volume --rsu 0 --pruning 0 --maxTxs 700 --minTxs $mtxs --blocktime 40 --group 1
		done
	done
done


printf "NAIVE Linear PoL"

for mtxs in  0 5 10 15
do
	printf "\n\n ${BRed}mtxs ~ $mtxs${NC}\n\n"
	
	for volume in  1
	do
		printf "\n\n ${BGreen}volume ~ $volume${NC}\n\n"
		
		for netsize in   100
		do
			python3 core.py --txs 10000 --netsize $netsize --printing False --dltmode linear --consensus near --map "HoustonHwyredblue"  --seed 1  --ref 2 --volume $volume --rsu 0 --pruning 0 --maxTxs 700 --minTxs $mtxs --blocktime 40 --group 2
		done
	done
done


printf "NAIVE DAG PoW"

for mtxs in  0 5 10 15
do
	printf "\n\n ${BRed}mtxs ~ $mtxs${NC}\n\n"
	
	for volume in  1
	do
		printf "\n\n ${BGreen}volume ~ $volume${NC}\n\n"
		
		for netsize in   100
		do
			python3 core.py --txs 10000 --netsize $netsize --printing False --dltmode dag --consensus individual --map "HoustonHwyredblue"  --seed 1  --ref 2 --volume $volume --rsu 0 --pruning 0 --maxTxs 700 --minTxs $mtxs --blocktime 40 --group 1
		done
	done
done


printf "NAIVE DAG PoL"

for mtxs in  0 5 10 15
do
	printf "\n\n ${BRed}mtxs ~ $mtxs${NC}\n\n"
	
	for volume in  1
	do
		printf "\n\n ${BGreen}volume ~ $volume${NC}\n\n"
		
		for netsize in   100
		do
			python3 core.py --txs 10000 --netsize $netsize --printing False --dltmode dag --consensus near --map "HoustonHwyredblue"  --seed 1  --ref 2 --volume $volume --rsu 0 --pruning 0 --maxTxs 700 --minTxs $mtxs --blocktime 40 --group 2
		done
	done
done