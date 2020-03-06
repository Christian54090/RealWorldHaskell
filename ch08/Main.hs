import SumFile
import ElfMagic
import Glob
import Text.Regex.Posix

vowelCount :: String -> Int
vowelCount = flip (=~) "[aeiouAEIOU]"