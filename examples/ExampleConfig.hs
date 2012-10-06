{-# LANGUAGE TemplateHaskell,NamedFieldPuns #-}
module Config (config) where

import ConfigBase

addPrefix :: Config -> Config
addPrefix r = let origName = name r in r {name = "hlogster." ++ origName}

config :: [Config]
config = map addPrefix $ concat [
  presentationBackup
  ]

presentationBackup :: [Config]
presentationBackup =
  [ FieldCounter {name = "presentation_backup.successful_backups", fields =
                     [Field {index = 6, match = "success"},
                      Field {index = 7, match = "presentation"}]}
  , RegexCounter {name = "presentation_backup.exceptions", regex = $(compileRegex "^\\[[^ ]+\\] [^ ]+ [^ ]+ INFO [^ ]+ .*[Ee]xception.*")}
  ]
