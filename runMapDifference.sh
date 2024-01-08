#!/bin/bash

BRed='\033[1;31m'         # Red
BGreen='\033[1;32m'       # Green
BYellow='\033[1;33m'      # Yellow
BBlue='\033[1;34m'        # Blue
NC='\033[0m' # No Color

for mapName in  "Houstonredblue" "HoustonHwyredblue"
do
	printf "\n\n ${BRed}mapName ~ $mapName${NC}\n\n"
	for agentNum in 150 200 #25 50 100 #10 15 20 25 30 40  50 60 70 80 90 #100
	#for agentNum in 10 75
	do	
	
		printf "\n\n ${BGreen}agentNum ~ $agentNum${NC}\n\n"
	
		#for blockTime in 110 120 130 140 150
		#do
			#printf "\n\n ${BBlue}blockTime ~ $blockTime${NC}\n\n"	
				
			for seednum in 1 
			#for seednum in 1 2 3
			do
			printf "\n\n ${BBlue}Seednum ~ $seednum${NC}\n\n"
			
				
				for group in 2 3 4 5 6 7
				do
					for refs in 2 3  
					do
					#printf "\n\n ${BGreen}refs ~ $refs${NC}\n\n"
					#python3 core.py --txs 20000 --netsize $agentNum --printing False --dltmode linear --consensus near --seed $seednum --map $mapName --references $refs --group $group
					python3 core.py --txs  20000 --netsize $agentNum --printing False --dltmode dag --consensus near --seed $seednum --map $mapName --references $refs --group $group
					#python3 core.py --txs 20000 --netsize $agentNum --printing False --dltmode dht --seed $seednum --map $mapName 
					#python3 core.py --txs 20000 --netsize $agentNum --printing False --dltmode linear --consensus individual --seed $seednum --map $mapName --blocktime $blockTime
					
					
				     #python3 core.py --txs  50000 --netsize $agentNum --printing False --dltmode linear --consensus individual --seed $seednum --map $mapName --blocktime $blockTime ##DONE
				#python3 core.py --txs 50000 --netsize $agentNum --printing False --dltmode dag --consensus individual --seed $seednum --map $mapName --references 3 --blockTime ##DONE
				
				#python3 core.py --txs 50000 --netsize $agentNum --printing False --dltmode linear --consensus near --seed $seednum --map $mapName
				#python3 core.py --txs 50000 --netsize $agentNum --printing False --dltmode dag --consensus near --seed $seednum --map $mapName --references 3

				#python3 core.py --txs 50000 --netsize $agentNum --printing False --dltmode hashgraph --seed $seednum --map $mapName
				#python3 core.py --txs 50000 --netsize $agentNum --printing False --dltmode dht --seed $seednum --map $mapName
				
				done
			done
		done
	done
done