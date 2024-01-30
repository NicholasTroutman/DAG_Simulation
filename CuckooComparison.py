##CUCKOO FILTER comparison
print("Comparing Cuckcoo Filters:\n")

##Step 1. create list of 10000 items

from random import choice
from string import ascii_lowercase, digits
import time
import math
from pympler import asizeof
chars = ascii_lowercase + digits
maxSize=2000
capacity=math.floor(maxSize/0.84)
lst = [''.join(choice(chars) for _ in range(10)) for _ in range(maxSize)]
lst2 = [''.join(choice(chars) for _ in range(10)) for _ in range(maxSize)]
#print(lst)
##Random string is DONE

#1. cuckoopy
#2. cuckoofilter
#3. cuckoo-filter
#4. scalable-cuckoo-filter



##EQUATIONS: https://www.cs.cmu.edu/~dga/papers/cuckoo-conext2014.pdf
#"the space-optimal bucket size depends on the target false positive rate ε: when ε > 0.002, having two entries per bucket yields slightly better results than using four entries per bucket; when ε decreases to 0.00001 < ε ≤ 0.002, four entries per bucket minimizes space"1
##False positive rate is 1%, then bucket is 2 ##FOR 4. old school version, it breaks at bucket=2
#"To retain the target false positive rate ε, the filter ensures 2b/2f ≤ ε, thus the minimal fingerprint size required is approximately: f ≥ log2(1/ε) + log2(2b)"1
#log2(1/0.01) + log2(2*2) = 8.64385618977 ~9
#FINGERPRINT IS 9
#"With k = 2 hash functions, the load factor α is 50% when the bucket size b = 1 (i.e., the hash table is directly mapped), but increases to 84%, 95% or 98% respectively using bucket size b = 2, 4 or 8."1
#b=2, then 0.85


## Step 2. Insert test
print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
print("\n\nInsert Test:\n")

bs=2
fs=9
#1. Cuckoopy
from cuckoopy import CuckooFilter
cf1 = CuckooFilter(capacity=capacity, bucket_size=bs, fingerprint_size=fs)
t0 = time.time()
for l in lst:
    cf1.insert(l)
    #cf1.contains(l)
t1 = time.time()
print("cuckoopy:\t\t",t1-t0)


#2. cuckoofilter
import cuckoofilter
cf2 = cuckoofilter.CuckooFilter(capacity=capacity,  fingerprint_size=fs)
t2 = time.time()
for l in lst:
    cf2.insert(l)
t3 = time.time()
print("cuckoofilter:\t\t",t3-t2)





#3. scalable-cuckoo-filter
    #CLASSIC
from cuckoo.filter import CuckooFilter
error_rate=0.01
cf3c = CuckooFilter(capacity=capacity, bucket_size=bs, error_rate=error_rate)
#fingerprint_size = int(math.ceil(math.log(1.0 / error_rate, 2) + math.log(2 * bucket_size, 2)))
#print(fingerprint_size)
t4 = time.time()
for l in lst:
    cf3c.insert(l)
t5 = time.time()
print("cuckoo.filter classic:\t",t5-t4)


    #bitarray
from cuckoo.filter import BCuckooFilter
#cuckoo = BCuckooFilter(capacity=capacity, error_rate=error_rate)
cf3b = BCuckooFilter(capacity=capacity, error_rate=error_rate, bucket_size=bs)
#fingerprint_size = int(math.ceil(math.log(1.0 / error_rate, 2) + math.log(2 * bucket_size, 2)))
#print(fingerprint_size)
t6 = time.time()
for l in lst:
    cf3b.insert(l)
t7 = time.time()
print("cuckoo.filter bitarray:\t", t7-t6)

    #scalable
from cuckoo.filter import ScalableCuckooFilter
cf3s = ScalableCuckooFilter(initial_capacity=capacity, error_rate=error_rate, bucket_size=bs)
# The fingerprint length is computed using the following formula:
fingerprint_size = int(math.ceil(math.log(1.0 / error_rate, 2) + math.log(2 * bs, 2)))
#print(fingerprint_size)
t8 = time.time()
for l in lst:
    cf3s.insert(l)
t9 = time.time()
print("cuckoo.filter scalable:\t", t9-t8)





#4. cuckoo-filter
from cuckoo_filter import CuckooFilter
cf4 = CuckooFilter(table_size=capacity, bucket_size=4, fingerprint_size=fs)

t10 = time.time()
for l in lst:
    #print(cf4.get_indices(l))
    cf4.insert(l)
t11 = time.time()
print("cuckoo_filter:\t\t", t11-t10)



##INCLUSION TEST:
print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
print("\n\nInclusion Speed Test:\n")


#0. inclusion TEst regular
t0=time.time()
for l in lst:
    l in lst
t1=time.time()
#print("cuckoopy size:\t\t",cf1.size)
print("List Inclusion\t\t",t1-t0)


#1. cuckopy
t0=time.time()
for l in lst:
    cf1.contains(l)
t1=time.time()
#print("cuckoopy size:\t\t",cf1.size)
print("cuckoopy\t\t",t1-t0)


#2. cuckoofilter
t0=time.time()
for l in lst:
    cf2.contains(l)
t1=time.time()
#print("cuckoopy size:\t\t",cf1.size)
print("cuckoofilter\t\t",t1-t0)



#3. cuckoo.filter
t0=time.time()
for l in lst:
    cf3c.contains(l)
t1=time.time()
#print("cuckoopy size:\t\t",cf1.size)
print("cuckoo.filter classic\t",t1-t0)


t0=time.time()
for l in lst:
    cf3b.contains(l)
t1=time.time()
#print("cuckoopy size:\t\t",cf1.size)
print("cuckoo.filter bitarray\t",t1-t0)

t0=time.time()
for l in lst:
    cf3s.contains(l)
t1=time.time()
#print("cuckoopy size:\t\t",cf1.size)
print("cuckoo.filter scalable\t",t1-t0)


#4. cuckoo_filter
t0=time.time()
for l in lst:
    cf4.__contains__(l)
t1=time.time()
#print("cuckoopy size:\t\t",cf1.size)
print("cuckoo_filter\t\t",t1-t0)








##INCLUSION ERROR TEST:
print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
print("\n\nInclusion Error Rate Test:\n")
#1. cuckopy
t0=time.time()
error=0
for l in lst2:
    if (cf1.contains(l) == False):
        error=error+1
        #print("WRONG")
t1=time.time()
#print("cuckoopy size:\t\t",cf1.size)
print("cuckoopy error:\t\t\t",error/maxSize)


#2. cuckoofilter
error=0
for l in lst2:
    if (cf2.contains(l) == False):
        error=error+1
        #print("WRONG")
t1=time.time()
#print("cuckoopy size:\t\t",cf1.size)
print("cuckoofilter error:\t\t",error/maxSize)



#3. cuckoo.filter
error=0
for l in lst2:
    if (cf3c.contains(l) == False):
        error=error+1
        #print("WRONG")
t1=time.time()
#print("cuckoopy size:\t\t",cf1.size)
print("cuckoo.filter classic error:\t",error/maxSize)


error=0
for l in lst2:
    if (cf3b.contains(l) == False):
        error=error+1
        #print("WRONG")
t1=time.time()
#print("cuckoopy size:\t\t",cf1.size)
print("cuckoo.filter bitarray error:\t",error/maxSize)



error=0
for l in lst2:
    if (cf3s.contains(l) == False):
        error=error+1
        #print("WRONG")
t1=time.time()
#print("cuckoopy size:\t\t",cf1.size)
print("cuckoo.filter scalaable error:\t",error/maxSize)


#4. cuckoo_filter
error=0
for l in lst2:
    if (cf4.__contains__(l) == False):
        error=error+1
        #print("WRONG")
t1=time.time()
#print("cuckoopy size:\t\t",cf1.size)
print("cuckoo_filter error:\t\t",error/maxSize)





##DELETION TEST:
print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
print("\n\nDELETION Speed Test:\n")
#1. cuckopy
t0=time.time()
for l in lst:
    cf1.delete(l)
t1=time.time()
#print("cuckoopy size:\t\t",cf1.size)
print("cuckoopy\t\t",t1-t0)


#2. cuckoofilter
t0=time.time()
for l in lst:
    cf2.delete(l)
t1=time.time()
#print("cuckoopy size:\t\t",cf1.size)
print("cuckoofilter\t\t",t1-t0)



#3. cuckoo.filter
t0=time.time()
for l in lst:
    cf3c.delete(l)
t1=time.time()
#print("cuckoopy size:\t\t",cf1.size)
print("cuckoo.filter classic\t",t1-t0)


t0=time.time()
for l in lst:
    cf3b.delete(l)
t1=time.time()
#print("cuckoopy size:\t\t",cf1.size)
print("cuckoo.filter bitarray\t",t1-t0)

t0=time.time()
for l in lst:
    cf3s.delete(l)
t1=time.time()
#print("cuckoopy size:\t\t",cf1.size)
print("cuckoo.filter scalable\t",t1-t0)


#4. cuckoo_filter
t0=time.time()
for l in lst:
    cf4.remove(l)
t1=time.time()
#print("cuckoopy size:\t\t",cf1.size)
print("cuckoo_filter\t\t",t1-t0)




## SIZE
import sys
print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
print("\n\nMEMORY SIZE (BYTES):\n")

#1. cuckoopy
print("cuckoopy\t\t",asizeof.asizeof(cf1))


#2. cuckoofilter
print("cuckoofilter\t\t",asizeof.asizeof(cf2))



#3. cuckoo.filter
print("cuckoo.filter classic\t",asizeof.asizeof(cf3c), sys.getsizeof(cf3c),"\t")

print("cuckoo.filter bitarray\t",asizeof.asizeof(cf3b))

print("cuckoo.filter scalable\t",asizeof.asizeof(cf3s))


#4. cuckoo_filter
print("cuckoo_filter\t\t",asizeof.asizeof(cf4))




##Properties
print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
print("\n\nCuckoo Filter Properties:\n")
#1 cuckopy
print("\ncuckopy properties:\n")
print("\tbucket_size:\t",cf1.bucket_size)
print("\tfingerprint_size:\t",cf1.fingerprint_size)
print("\tcapacity:\t",cf1.capacity)


#2 cuckoofilter
print("\ncuckoofilter properties:\n")
#print("\tbucket_size:\t",cf2.__buckets )
#print("\tfingerprint_size:\t",cf2.__fingerprint_size )
#print("\tcapacity:\t",cf2.__capacity )
print(cf2.__repr__ )

#3. cuckoo.filter
print("\ncuckoo.filter properties:\n")
print("\tClassic:\t",cf3c.__repr__())
print("\tBitarray:\t",cf3b.__repr__())
print("\tScalable:\t",cf3s.__repr__())


#4. cuckoo_filter
print("\ncuckoo_filter properties:\n")
print("\tBucket Size:\t",cf4.bucket_size)
print("\tFingerprint Size:\t",cf4.fingerprint_size)


###SIZE
print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
print("\n\nCuckoo Filter THEORETICAL SIZE:\n")
print("\tCapacity:\t\t",capacity)
print("\tBucket Size:\t\t",bs)
print("\tFingerprint Size:\t",fs)
print("\tPer Bucket:\t\t",bs*fs)
print("\tTOTAL MEMORY (Bits):\t",bs*fs*capacity)
print("\tTOTAL MEMORY (Bytes):\t",bs*fs*capacity/8)
print("\tTOTAL MEMORY (KB):\t",bs*fs*capacity/1024)
