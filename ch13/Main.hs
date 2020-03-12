import qualified Data.Map as M
import PasswdA1
import PasswdMap
import BuildMap

myLookup _ [] = Nothing
myLookup key ((thisKey,thisVal):rest) =
  if key == thisKey
     then Just thisVal
     else myLookup key rest