module GMachineCompiler (
 GmCompiler,
 GmEnvironment,
 compile
) where


import Heap
import Core
import GMachine

import Data.List (mapAccumL)


compile :: CoreProgram -> GmState
compile program
 = (initialCode, [], heap, globals, statInitial)
 where (heap, globals) = buildInitialHeap program

buildInitialHeap :: CoreProgram -> (GmHeap, GmGlobals)
buildInitialHeap program
 = mapAccumL allocateSc hInitial compiled
 where compiled = map compileSc program

type GmCompiledSC = (Name, Int, GmCode)

allocateSc :: GmHeap -> GmCompiledSC -> (GmHeap, (Name, Addr))
allocateSc heap (name, nargs, instns)
 = (heap', (name, addr))
 where (heap', addr) = hAlloc heap (NGlobal nargs instns)

initialCode :: GmCode
initialCode = [Pushglobal "main", Unwind]

compileSc :: (Name, [Name], CoreExpr) -> GmCompiledSC
compileSc (name, env, body)
 = (name, length env, compileR body (zip env [0..]))

compileR :: GmCompiler
--compileR e env = compileC e env ++ [Slide (length env + 1), Unwind]
compileR e env = compileC e env ++ [Update d, Pop d, Unwind]
 where d = length env

type GmCompiler = CoreExpr -> GmEnvironment -> GmCode

type GmEnvironment = ASSOC Name Int

compileC :: GmCompiler
compileC (EVar v)    env
 | elem v (aDomain env) = [Push n]
 | otherwise            = [Pushglobal v]
 where n = aLookup env v (error "Can't happen")
compileC (ENum n)    evn = [Pushint n]
compileC (EAp e1 e2) env = compileC e2 env ++
                           compileC e1 (argOffset 1 env) ++
                           [Mkap]

argOffset :: Int -> GmEnvironment -> GmEnvironment
argOffset n env = map (\e -> case e of (v,m) -> (v,n+m)) env
