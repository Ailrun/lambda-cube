module LambdaCube.TestUtil where

const2 :: a -> b -> c -> a
const2 x _ _ = x

makeDefaultTitle :: String -> String -> String -> String
makeDefaultTitle fName left right =
  concat
  [ fName
  , " ("
  , left
  , ") should be ("
  , right
  , ")"
  ]
