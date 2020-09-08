module Network.Curl.Prim.Mem

import Derive.Prim
%language ElabReflection

%runElab makeHasIO "foreign_alloc" Export ["scheme:foreign-alloc"]
  `[prim_foreign_alloc : (bytes : Int) -> PrimIO AnyPtr] --`

%runElab makeHasIO "foreign_free" Export ["scheme:foreign-free"]
  `[prim_foreign_free : AnyPtr -> PrimIO ()] --`


%runElab makeHasIO "foreign_ref_int" Export
   ["scheme:(lambda (addr offset) (foreign-ref 'int addr offset))"]
  `[prim_foreign_ref_int : AnyPtr -> (offset : Int) -> PrimIO Int] --`

%runElab makeHasIO "foreign_set_int" Export
   ["scheme:(lambda (addr offset val) (foreign-set! 'int addr offset val))"]
  `[prim_foreign_set_int : AnyPtr -> (offset : Int) -> (val : Int) -> PrimIO Int] --`

export
alloc : HasIO io => (bytes : Int) -> (AnyPtr -> io a) -> io a
alloc size act = do ptr <- foreign_alloc size
                    res <- act ptr
                    foreign_free ptr
                    pure res

main : IO ()
main = do
  ptr <- foreign_alloc 30
  foreign_set_int ptr 0 12
  v <- foreign_ref_int ptr 0
  printLn v
  foreign_set_int ptr 0 15
  v <- foreign_ref_int ptr 0
  printLn v
  foreign_free ptr
