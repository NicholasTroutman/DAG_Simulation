#!/bin/bash

BRed='\033[1;31m'         # Red
BGreen='\033[1;32m'       # Green
BYellow='\033[1;33m'      # Yellow
BBlue='\033[1;34m'        # Blue
NC='\033[0m' # No Color

for mapName in "Houstonredblue" #"HoustonHwyredblue" 
do
	printf "\n\n ${BRed}mapName ~ $mapName${NC}\n\n"
	for agentNum in 25 50 100
	do	
	
		printf "\n\n ${BGreen}agentNum ~ $agentNum${NC}\n\n"
	
			
		for seednum in 1 
		#for seednum in 1 2 3
		do
		
			for group in  2 3 4 
			do
			printf "\n\n ${BBlue}group ~ $group${NC}\n\n"
				for refs in 2 3 4
				do
				
				printf "\n\n ${BGreen}refs ~ $refs${NC}\n\n"
				python3 core.py --txs 70000 --netsize $agentNum --printing False --dltmode dag --consensus near --seed $seednum --map $mapName --group $group --references $refs
			
	
			#python3 core.py --txs  50000 --netsize $agentNum --printing False --dltmode linear --consensus individual --seed $seednum --map $mapName --blocktime $blockTime
			#python3 core.py --txs 50000 --netsize $agentNum --printing False --dltmode dag --consensus individual --seed $seednum --map $mapName --references 3 --blockTime

			#python3 core.py --txs 50000 --netsize $agentNum --printing False --dltmode linear --consensus near --seed $seednum --map $mapName
			#python3 core.py --txs 50000 --netsize $agentNum --printing False --dltmode dag --consensus near --seed $seednum --map $mapName --references 3

			#python3 core.py --txs 50000 --netsize $agentNum --printing False --dltmode hashgraph --seed $seednum --map $mapName
			#python3 core.py --txs 50000 --netsize $agentNum --printing False --dltmode dht --seed $seednum --map $mapName
				done
			done
		done
		
	done
done
