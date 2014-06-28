module Core (
 Expr(..),
 IsRec, recursive, nonRecursive,
 bindersOf, rhssOf, isAtomicExpr,
 Alter, Program, ScDefn,
 CoreAlt, CoreExpr, CoreProgram, CoreScDefn
) where

import Heap

data Expr a
 = EVar Name                 -- Variables
 | ENum Int                  -- Numbers
 | EConstr Int Int           -- Constructor tag arity
 | EAp (Expr a) (Expr a)     -- Applications
 | ELet                      -- Let(rec) expressions
      IsRec                  --   boolean with True = recursive
      [(a, Expr a)]          --   Definitions
      (Expr a)               --   Body of let(rec)
 | ECase                     -- Case expressions
      (Expr a)               --   Expression to scrutinise
      [Alter a]              --   Alternatives
 | ELam [a] (Expr a)         -- Lambda abstractions
   deriving Show

type IsRec = Bool

recursive :: IsRec
recursive = True
nonRecursive :: IsRec
nonRecursive = False

bindersOf :: [(a,b)] -> [a]
bindersOf defns = map fst defns

rhssOf    :: [(a,b)] -> [b]
rhssOf    defns = map snd defns

isAtomicExpr :: Expr a -> Bool
isAtomicExpr (EVar v) = True
isAtomicExpr (ENum v) = True
isAtomicExpr _        = False

type Alter a = (Int, [a], Expr a)
type Program a = [ScDefn a]
type ScDefn a = (Name, [a], Expr a)

type CoreAlt = Alter Name
type CoreExpr = Expr Name
type CoreProgram = Program Name
type CoreScDefn = ScDefn Name


