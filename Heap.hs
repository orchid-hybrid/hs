module Heap (
 Name,
 Addr,
 Heap,
 hInitial, hAlloc, hUpdate, hFree, hLookup,
 hAddresses, hSize,
 hNull, hIsnull,
 
 ASSOC,
 aLookup, aDomain, aRange, aEmpty
) where

import Data.List

type Name = String

type Addr = Int
type Heap a = (Int, [Int], [(Int, a)])

hInitial :: Heap a
hInitial = (0,      [1..],  [])

hAlloc :: Heap a -> a -> (Heap a, Addr)
hAlloc (size, (next:free), cts) n   = ((size+1, free,   (next,n) : cts),next)

hUpdate :: Heap a -> Addr -> a -> Heap a
hUpdate (size, free,        cts) a n = (size,   free,   (a,n) : remove cts a)

hFree :: Heap a -> Addr -> Heap a
hFree (size, free,        cts) a   = (size-1, a:free, remove cts a)

hLookup :: Heap a -> Addr -> a
hLookup (size,free,cts) a
 = aLookup cts a (error ("can't find node " ++ showaddr a ++ " in heap"))

hAddresses :: Heap a -> [Addr]
hAddresses (size, free, cts) = [addr | (addr, node) <- cts]

hSize :: Heap a -> Int
hSize (size, free, cts) = size

hNull :: Addr
hNull = 0

hIsnull :: Addr -> Bool
hIsnull a = a==0



shownum = show
showaddr = show

remove [] a = error ("Attempt to update or free nonexistent address #" ++ shownum a)
remove ((a',n):cts) a | a == a' = cts
                      | a /= a' = (a',n) : remove cts a


type ASSOC k v = [(k,v)]

aLookup :: (Eq a) => ASSOC a b -> a -> b -> b
aLookup []         k' def           = def
aLookup ((k,v):bs) k' def | k == k' = v
                          | k /= k' = aLookup bs k' def
aDomain :: ASSOC a b -> [a]
aDomain alist = [key | (key,val) <- alist]

aRange :: ASSOC a b -> [b]
aRange  alist = [val | (key,val) <- alist]

aEmpty = []

