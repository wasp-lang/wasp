module Command.Call where

data Call = New String -- project name
          | Start
          | Clean
          | Compile
          | Db [String] -- db args
          | Build
          | Version
          | Unknown [String] -- all args
