#!/bin/bash

BRed='\033[1;31m'         # Red
BGreen='\033[1;32m'       # Green
BYellow='\033[1;33m'      # Yellow
BBlue='\033[1;34m'        # Blue
NC='\033[0m' # No Color



printf "The Whole DLT"

for mtxs in  20 25 30 35 40 45 50 55 60 65 70 75 80 90 100 110 120 130
do
	printf "\n\n ${BRed}mtxs ~ $mtxs${NC}\n\n"
	
	for volume in  1
	do
		printf "\n\n ${BGreen}volume ~ $volume${NC}\n\n"
		
		for netsize in  25 50 100
		do
			printf "\n\n ${BBlue}agents ~ $netsize${NC}\n\n"
			python3 core.py --txs 10000 --netsize $netsize --printing False --dltmode dag --consensus individual --map "HoustonHwyredblue"  --seed 1  --ref 2 --volume $volume --rsu 0 --pruning 0 --blocktime 30 --maxTxs $mtxs
		done
	done
done

printf "Balance"


for mtxs in  20 25 30 35 40 45 50 55 60 65 70 75 80 90 100 110 120 130
do
	printf "\n\n ${BRed}mtxs ~ $mtxs${NC}\n\n"
	
	for volume in  1
	do
		printf "\n\n ${BGreen}volume ~ $volume${NC}\n\n"
		
		for netsize in  25 50 100
		do
			printf "\n\n ${BBlue}agents ~ $netsize${NC}\n\n"
			python3 core.py --txs 10000 --netsize $netsize --printing False --dltmode dag --consensus individual --map "HoustonHwyredblue"  --seed 1  --ref 2 --volume $volume --rsu 0 --pruning 0 --balance 1 --blocktime 30 --maxTxs $mtxs
		done
	done
done



printf "pruning"

for mtxs in  20 25 30 35 40 45 50 55 60 65 70 75 80 90 100 110 120 130
do
	printf "\n\n ${BRed}mtxs ~ $mtxs${NC}\n\n"
	
	for volume in  1
	do
		printf "\n\n ${BGreen}volume ~ $volume${NC}\n\n"
		
		for netsize in  25 50 100
		do
			python3 core.py --txs 10000 --netsize $netsize --printing False --dltmode dag --consensus individual --map "HoustonHwyredblue"  --seed 1  --ref 2 --volume $volume --rsu 0 --pruning 1 --blocktime 30 --maxTxs $mtxs
		done
	done
done

print "rsus"


for mtxs in  20 25 30 35 40 45 50 55 60 65 70 75 80 90 100 110 120 130
do
	printf "\n\n ${BRed}mtxs ~ $mtxs${NC}\n\n"
	
	for volume in  1
	do
		printf "\n\n ${BGreen}volume ~ $volume${NC}\n\n"
		
		for netsize in  25 50 100
		do
			python3 core.py --txs 10000 --netsize $netsize --printing False --dltmode dag --consensus individual --map "HoustonHwyredblue"  --seed 1  --ref 2 --volume $volume --rsu 6 --pruning 0 --blocktime 30 --maxTxs $mtxs
		done
	done
done