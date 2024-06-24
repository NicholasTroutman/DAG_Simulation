#!/bin/bash

BRed='\033[1;31m'         # Red
BGreen='\033[1;32m'       # Green
BYellow='\033[1;33m'      # Yellow
BBlue='\033[1;34m'        # Blue
NC='\033[0m' # No Color


printf "Unit Tests:\n\n"

printf "Linear Blockchain Basics: \n"

python3 core.py --txs 100 --netsize 2 --printing False --dltmode linear --consensus individual --map "HoustonHwyredblue"  --seed 1 --volume 0 --rsu 0 --pruning 0 --blocktime 40  --p2p 0 --lambda 1

python3 core.py --txs 100 --netsize 5 --printing False --dltmode linear --consensus individual --map "HoustonHwyredblue"  --seed 1 --volume 0 --rsu 0 --pruning 0 --blocktime 40  --p2p 0 --lambda 1

python3 core.py --txs 100 --netsize 10 --printing False --dltmode linear --consensus individual --map "HoustonHwyredblue"  --seed 1 --volume 0 --rsu 0 --pruning 0 --blocktime 40  --p2p 0 --lambda 1

printf "DAG Blockchain Basics: \n"

python3 core.py --txs 100 --netsize 2 --printing False --dltmode dag --consensus individual --map "HoustonHwyredblue"  --seed 1 --volume 0 --rsu 0 --pruning 0 --blocktime 40  --p2p 0 --lambda 1

python3 core.py --txs 100 --netsize 5 --printing False --dltmode dag --consensus individual --map "HoustonHwyredblue"  --seed 1 --volume 0 --rsu 0 --pruning 0 --blocktime 40  --p2p 0 --lambda 1

python3 core.py --txs 100 --netsize 10 --printing False --dltmode dag --consensus individual --map "HoustonHwyredblue"  --seed 1 --volume 0 --rsu 0 --pruning 0 --blocktime 40  --p2p 0 --lambda 1


printf "HashGraph Blockchain Basics: \n"

python3 core.py --txs 100 --netsize 2 --printing False --dltmode hashgraph  --map "HoustonHwyredblue"  --seed 1 --volume 0 --rsu 0 --pruning 0 --blocktime 40  --p2p 0 --lambda 1

python3 core.py --txs 100 --netsize 5 --printing False --dltmode hashgraph  --map "HoustonHwyredblue"  --seed 1 --volume 0 --rsu 0 --pruning 0 --blocktime 40  --p2p 0 --lambda 1

python3 core.py --txs 100 --netsize 10 --printing False --dltmode hashgraph  --map "HoustonHwyredblue"  --seed 1 --volume 0 --rsu 0 --pruning 0 --blocktime 40  --p2p 0 --lambda 1

printf "TreeChain Blockchain Basics: \n"

python3 core.py --txs 100 --netsize 2 --printing False --dltmode dht --map "HoustonHwyredblue"  --seed 1 --volume 0 --rsu 0 --pruning 0 --blocktime 40  --p2p 0 --lambda 1

python3 core.py --txs 100 --netsize 5 --printing False --dltmode dht --map "HoustonHwyredblue"  --seed 1 --volume 0 --rsu 0 --pruning 0 --blocktime 40  --p2p 0 --lambda 1

python3 core.py --txs 100 --netsize 10 --printing False --dltmode dht  --map "HoustonHwyredblue"  --seed 1 --volume 0 --rsu 0 --pruning 0 --blocktime 40  --p2p 0 --lambda 1

printf "PoL Test: \n"

python3 core.py --txs 100 --netsize 5 --printing False --dltmode linear --consensus near --group 2 --map "HoustonHwyredblue"  --seed 1 --volume 0 --rsu 0 --pruning 0 --blocktime 40  --p2p 0 --lambda 1

python3 core.py --txs 100 --netsize 5 --printing False --dltmode dag --consensus near  --group 2 --map "HoustonHwyredblue"  --seed 1 --volume 0 --rsu 0 --pruning 0 --blocktime 40  --p2p 0 --lambda 1

printf "Lambda Test: \n"

python3 core.py --txs 100 --netsize 5 --printing False --dltmode linear --consensus near --group 2  --map "HoustonHwyredblue"  --seed 1 --volume 0 --rsu 0 --pruning 0 --blocktime 40  --p2p 0 --lambda 2

python3 core.py --txs 100 --netsize 5 --printing False --dltmode dag --consensus near --group 2  --map "HoustonHwyredblue"  --seed 1 --volume 0 --rsu 0 --pruning 0 --blocktime 40  --p2p 0 --lambda 5


printf "Group Test: \n"

python3 core.py --txs 100 --netsize 50 --printing False --dltmode linear --consensus near --group 3  --map "HoustonHwyredblue"  --seed 1 --volume 0 --rsu 0 --pruning 0 --blocktime 40  --p2p 0 --lambda 2

python3 core.py --txs 100 --netsize 50 --printing False --dltmode dag --consensus near --group 4  --map "HoustonHwyredblue"  --seed 1 --volume 0 --rsu 0 --pruning 0 --blocktime 40  --p2p 0 --lambda 5
