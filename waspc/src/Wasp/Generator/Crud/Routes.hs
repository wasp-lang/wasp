module Wasp.Generator.Crud.Routes where

import Data.List (intercalate)

getPath :: String
getPath = "get"

makeGetRoute :: String -> String
makeGetRoute name = makeCrudRoute name getPath

getAllPath :: String
getAllPath = "getAll"

makeGetAllRoute :: String -> String
makeGetAllRoute name = makeCrudRoute name getAllPath

createPath :: String
createPath = "create"

makeCreateRoute :: String -> String
makeCreateRoute name = makeCrudRoute name createPath

updatePath :: String
updatePath = "update"

makeUpdateRoute :: String -> String
makeUpdateRoute name = makeCrudRoute name updatePath

deletePath :: String
deletePath = "delete"

makeDeleteRoute :: String -> String
makeDeleteRoute name = makeCrudRoute name deletePath

makeCrudRoute :: String -> String -> String
makeCrudRoute name path = intercalate "/" ["crud", name, path]
