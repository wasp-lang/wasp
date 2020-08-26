module Generator.WebAppGenerator.QueryGenerator
    ( genQueries
    ) where

import Data.Maybe (fromJust)
import Data.Aeson ((.=), object)
import qualified Path as P

import Wasp (Wasp)
import qualified Wasp
import qualified Wasp.Query
import Generator.FileDraft (FileDraft)
import qualified Generator.ServerGenerator as ServerGenerator
import qualified Generator.ServerGenerator.QueryGenerator as ServerGenerator.QueryGenerator
import qualified Generator.WebAppGenerator.Common as C

genQueries :: Wasp -> [FileDraft]
genQueries wasp = concat
    [ map (genQuery wasp) (Wasp.getQueries wasp)
    ]

genQuery :: Wasp -> Wasp.Query.Query -> FileDraft
genQuery _ query = C.makeTemplateFD tmplFile dstFile (Just tmplData)
  where
    tmplFile = C.asTmplFile [P.relfile|src/queries/_query.js|]

    -- | TODO: fromJust here could fail if there is some problem with the name, we should handle this.
    dstFile = C.asWebAppFile $ [P.reldir|src/queries/|] P.</> fromJust (P.parseRelFile dstFileName)

    dstFileName = Wasp.Query._name query ++ ".js"

    tmplData = object
        [ "queryFnName" .= Wasp.Query._name query
        , "queryRoute" .=
            (ServerGenerator.queriesRouteInRootRouter
             ++ "/" ++ ServerGenerator.QueryGenerator.queryRouteInQueriesRouter query)
        ]

