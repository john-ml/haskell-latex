import Latex

import qualified Data.Set as Set

main :: IO ()
main = do
  print . doc $ "a" + ("b" + 3 / 5 + 4) / "x"
