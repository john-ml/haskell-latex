import Latex

main :: IO ()
main = do
  let asDoc a = a :: Doc
  print . asDoc $ 1 + 1 * 2
  print . asDoc $ 1 + 1 === 2 \/ 2 + 2 === 4
  print . asDoc $ 1 ∧ 2 ∨ 3
  print . asDoc $ (1 ∨ 2) ∧ 3
  print . asDoc $ (1 + 1 === 2) /\ (2 + 2 =/= 1)
  print . asDoc $ forall x (forall y (y + 1 === "2") ===> x + 2 === 1)
  print . asDoc . forall "x S T" $ x ∈ "S" ∪ "T" <==> (x ∈ "S") ∨ (x ∈ "T")
  print . asDoc $ (1 + 1) / 2 + 3 + 0.5 - 4/5
  print . doc $ do
    para $ do
      "this is the first sentence of a paragraph."
      "this is the second sentence of a paragraph."
      "this is the third sentence of a paragraph."
    para $ do
      "this is another paragraph. here is a math fact: "
      [1 + 2 === 3]
      "semicolons can be used to make 'inline' math. e.g."
      "if"; [1 + 1 === 0]; "then"; [0 + 0 === 1]
  print . doc $ do
    figure "test-fig" "This is a test figure." $ do
      "This is a test figure body. "
    para $ do
      "this is some text later on that references figure "; ref "test-fig"; "."
    chart "test-img" "test_img.jpg" $ do
      "This is a test image. It should be scaled down to the line width if necessary"
    para $ "this is just a picture splatted into the pdf:"
    pic "test_img.jpg"
    enumerate $ do
      "item 1"
      "item 2"
      enumerate $ do
        "item 3.1"
        "item 3.2"
        itemize $ do
          "item 3.3.1"
          "item 3.3.2"
          "item 3.3.3"
      "item 4"
