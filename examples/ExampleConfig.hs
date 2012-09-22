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
  , RegexCounter {name = "timing", regex = $(compileRegex "^[^ ]+ [^ ]+ [^ ]+ storage INFO [^ ]+ timing ([^ ]+) ([0-9.]+) ([^ ]+) .*$")}
  , RegexCounter {name = "item_read", regex = $(compileRegex "^[^ ]+ [^ ]+ [^ ]+ storage INFO [0-9]+:item_read")}
  ]
