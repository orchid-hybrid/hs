module Testing where

import Core
import GMachineCompiler
import GMachineEvaluator

run = mapM_ print . map foo . eval . compile
 where foo (code, stack, heap, globals, stats) = (code, stack, bar heap)
       bar (x, _, t) = (x,t)
