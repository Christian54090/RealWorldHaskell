module Do where

{-

doNotation1 =
  do act

translated1 =
  act

doNotation2 =
  do act1
     act2
     -- etc. --
     actN

translated2 =
  act1 >>
  act2 >>
  -- etc --
  actN

doNotation3 =
  do pattern <- act1
     act2
     -- etc --
     actN

translated3 =
  let f pattern = do act2
                     let f pattern = do act2
                     actN
      f _       = fail "..."
  in act1 >>= f

-}

