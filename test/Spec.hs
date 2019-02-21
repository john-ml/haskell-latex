import Latex

import qualified Data.Set as Set

main :: IO ()
main = do
  putStrLn $ show ("a" + ("b" + 3 / 5 + 4) / "x" :: Document)
