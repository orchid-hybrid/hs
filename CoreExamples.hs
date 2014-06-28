module CoreExamples where

import Core

preludeDefs :: CoreProgram
preludeDefs
  = [ ("i", ["x"], EVar "x"),
      ("k", ["x","y"], EVar "x"),
      ("k1",["x","y"], EVar "y"),
      ("s", ["f","g","x"], EAp (EAp (EVar "f") (EVar "x"))
                               (EAp (EVar "g") (EVar "x"))),
      ("compose", ["f","g","x"], EAp (EVar "f")
                                      (EAp (EVar "g") (EVar "x"))),
      ("twice", ["f"], EAp (EAp (EVar "compose") (EVar "f")) (EVar "f")) ]
