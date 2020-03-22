module StackStack where

import Control.Monad.State

type Foo = StateT Int (State String)
type Bar = ReaderT Bool Foo

outerPut :: Int -> Foo ()
outerPut = put

innerPut :: String -> Foo ()
innerPut lift . put

barPut :: String -> Bar ()
barPut = lift . lift . put

