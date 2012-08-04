module ConfigBase where

data Config =
  EventCounter {name :: String, category :: String, event :: String} |
  FieldCounter {name :: String, fields :: [FieldSpec]}

data FieldSpec = Field { index :: Int, match :: String }

