{-# LANGUAGE TemplateHaskell #-}
module Config (config) where

import ConfigBase


config :: [Config]
config =
  [ EventCounter {name = "presentations_delete", category = "usage", event = "presentations_delete"}
  , EventCounter {name = "feature.label_zoom", category = "feature", event = "label_zoom"}
  , FieldCounter {name = "something.different",
                  fields = [Field {index = 4, match = "storage"},
                            Field {index = 7, match = "and_now_for_something_completely_different"}]}
  , RegexCounter {name = "regexExample", regex = $(compileRegex "^[^ ]+ [^ ]+ [^ ]+ storage INFO [^ ]+ timing ([^ ]+) ([0-9.]+) ([^ ]+) .*$")}
  ]
