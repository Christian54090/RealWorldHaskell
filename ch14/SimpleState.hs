module SimpleState where

type SimpleState s a = s -> (a, s)

fakeState :: s -> (a, s)
fakeState = undefined

type StringState a = SimpleState String a

returnSt :: a -> SimpleState s a
returnSt a = \s -> (a, s)

returnAlt :: a -> SimpleState s a
returnAlt a s = (a, s)

bindSt :: (SimpleState s a) -> (a -> SimpleState s b) -> SimpleState s b
bindSt m f = \s -> let (a, s') = m s
                   in  (f a) s'
-- m == step
-- f == makeStep
-- s == oldState

bindAlt :: (SimpleState s a) -> (a -> SimpleState s b) -> SimpleState s b
bindAlt step makeStep oldState = (makeStep result) newState
  where (result, newState) = step oldState -- :: s -> (a, s)

getSt :: SimpleState s s
getSt = \s -> (s, s)

putSt :: s -> SimpleState s ()
putSt s = \_ -> ((), s)
