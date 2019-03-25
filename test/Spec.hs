import Latex

main :: IO ()
main = do
  print . doc $ 1 + 1 * 2
  print . doc $ 1 + 1 === 2 \/ 2 + 2 === 4
  print . doc $ 1 ∧ 2 ∨ 3
  print . doc $ (1 ∨ 2) ∧ 3
  print . doc $ (1 + 1 === 2) /\ (2 + 2 =/= 1)
  print . doc $ forall "x" (forall "y" ("y" + 1 === "2") ===> "x" + 2 === 1)
  print . doc . forall "x S T" $ "x" ∈ "S" ∪ "T" <==> ("x" ∈ "S") ∨ ("x" ∈ "T")
