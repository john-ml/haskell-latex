import Latex

import qualified Data.Set as Set

main :: IO ()
main = do
  putStrLn . show $ Document [(Verbatim "", Precedence Set.empty 0)]
