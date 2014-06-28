module GMachine (
  GmState,
  GmCode, getCode, putCode, Instruction(..),
  GmStack, getStack, putStack,
  GmHeap, getHeap, putHeap, Node(..),
  GmGlobals, getGlobals,
  GmStats, statInitial, statIncSteps, statGetSteps, getStats, putStats
) where

import Heap
import Core

type GmState
 = (GmCode,    -- Current instruction stream
 GmStack,      -- Current stack
 GmHeap,       -- Heap of Nodes
 GmGlobals,    -- Global addresses in heap
 GmStats)      -- Statistics

type GmCode = [Instruction]

getCode :: GmState -> GmCode
getCode (i, stack, heap, globals, stats) = i

putCode :: GmCode -> GmState -> GmState
putCode i (_, stack, heap, globals, stats) = (i, stack, heap, globals, stats)

data Instruction
    = Unwind
    | Pushglobal Name
    | Pushint Int
    | Push Int
    | Mkap
--    | Slide Int
    | Update Int
    | Pop Int
      deriving (Eq, Show)

type GmStack = [Addr]

getStack :: GmState -> GmStack
getStack (i, stack, heap, globals, stats) = stack

putStack :: GmStack -> GmState -> GmState
putStack stack (i, _, heap, globals, stats) = (i, stack, heap, globals, stats)

type GmHeap = Heap Node

getHeap :: GmState -> GmHeap
getHeap (i, stack, heap, globals, stats) = heap

putHeap :: GmHeap -> GmState -> GmState
putHeap heap (i, stack, _, globals, stats) = (i, stack, heap, globals, stats)

data Node
    = NNum Int              -- Numbers
    | NAp Addr Addr         -- Applications
    | NGlobal Int GmCode    -- Globals
    | NInd Addr             -- Indirections
      deriving (Eq, Show)

type GmGlobals = ASSOC Name Addr

getGlobals :: GmState -> GmGlobals
getGlobals (i, stack, heap, globals, stats) = globals

type GmStats = Integer

statInitial = 0
statIncSteps s = s + 1
statGetSteps s = s

getStats :: GmState -> GmStats
getStats (i, stack, heap, globals, stats) = stats

putStats :: GmStats -> GmState -> GmState
putStats stats (i, stack, heap, globals, _) = (i, stack, heap, globals, stats)
