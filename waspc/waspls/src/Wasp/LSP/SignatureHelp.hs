{-# LANGUAGE FlexibleInstances #-}

module Wasp.LSP.SignatureHelp
  ( getSignatureHelpAtPosition,
  )
where

import Control.Lens ((^.))
import Control.Monad (guard)
import Control.Monad.Log.Class (MonadLog, logM)
import Control.Monad.State.Class (MonadState, gets)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT))
import qualified Data.HashMap.Strict as M
import Data.List (elemIndex, intersperse)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.String (IsString (fromString))
import qualified Data.Text as Text
import qualified Language.LSP.Types as LSP
import Text.Printf (printf)
import Wasp.Analyzer.Parser.CST.Traverse (Traversal, fromSyntaxForest)
import Wasp.Analyzer.Type (Type)
import qualified Wasp.Analyzer.Type as Type
import Wasp.LSP.ServerState (ServerState, cst, currentWaspSource)
import Wasp.LSP.Syntax (lspPositionToOffset, toOffset)
import Wasp.LSP.TypeInference (ExprKey (Key), findExprPathAtLocation, findTypeForPath)

-- TODO: overhaul doc comments based on latest explanation i have somewhere else

-- | Get 'LSP.SignatureHelp' at a position in the wasp file. The signature
-- displays the type of the "container" that the position is in, if any.
--
-- A container is an expression that holds other values, such as dictionaries
-- and lists.
--
-- The parameter field of the signature is used for which part of the container
-- the position is within, such as a key for a dictionary.
getSignatureHelpAtPosition ::
  (MonadState ServerState m, MonadLog m) =>
  LSP.Position ->
  m LSP.SignatureHelp
getSignatureHelpAtPosition position = do
  src <- gets (^. currentWaspSource)
  gets (^. cst) >>= \case
    Nothing ->
      -- No CST in the server state, can't create a signature.
      return emptyHelp
    Just syntax -> do
      let offset = lspPositionToOffset src position
      let location = toOffset offset (fromSyntaxForest syntax)
      findSignatureAtLocation src location >>= \case
        Nothing -> do
          logM "[getSignatureHelpAtPosition] no type hint to provide signature for"
          return emptyHelp
        Just signature -> do
          logM "[getSignatureHelpAtPosition] type hint found"
          let signatureFragments = signatureToFragments signature
          let params = getParamsFromFragments signatureFragments
          let activeParam = signatureParam signature >>= findActiveParameterIndex signatureFragments
          logM $ "[getSignatureHelpAtPosition] at param idx = " ++ show activeParam ++ " params = " ++ show params
          return $
            LSP.SignatureHelp
              { _signatures =
                  LSP.List
                    [ LSP.SignatureInformation
                        { _parameters = Just $ LSP.List params,
                          -- NOTE: VSCode higlights the 0th parameter if
                          -- activeParameter is Nothing, so it's set to -1 when
                          -- we don't want any param highlighted.
                          _activeParameter = Just $ fromMaybe (-1) activeParam,
                          _label = Text.pack $ showFragments signatureFragments,
                          _documentation = Nothing
                        }
                    ],
                _activeSignature = Just 0,
                _activeParameter = Nothing
              }
  where
    emptyHelp =
      LSP.SignatureHelp
        { _signatures = LSP.List [],
          _activeSignature = Nothing,
          _activeParameter = Nothing
        }

-- | Describes the expected type at a location.
data Signature
  = -- | Inside a container, but not at any parameter inside of it.
    InContainer !Type
  | -- | Inside a container at the parameter specified by the 'ExprKey'.
    AtParam !Type !ExprKey
  deriving (Eq, Show)

signatureType :: Signature -> Type
signatureType (InContainer typ) = typ
signatureType (AtParam typ _) = typ

signatureParam :: Signature -> Maybe ExprKey
signatureParam (InContainer _) = Nothing
signatureParam (AtParam _ key) = Just key

-- | TODO
--
-- TODO: this never gives 'AtParam' when the location is inside a list, but
-- it probably always should.
findSignatureAtLocation ::
  (MonadLog m) =>
  String ->
  Traversal ->
  m (Maybe Signature)
findSignatureAtLocation src location = runMaybeT $ do
  exprPath <- hoistMaybe $ findExprPathAtLocation src location
  lift $ logM $ "[SignatureHelp] at expr path " ++ show exprPath
  guard $ not $ null exprPath
  case exprPath of
    [path] -> InContainer <$> hoistMaybe (findTypeForPath [path])
    path -> do
      -- Using init/last here is OK since we know @path@ has at least 2 elements.
      tipType <- hoistMaybe $ findTypeForPath path
      parentType <- hoistMaybe $ findTypeForPath $ init path
      if isContainerType tipType
        then return $ InContainer tipType
        else return $ AtParam parentType (last path)
  where
    isContainerType :: Type -> Bool
    isContainerType (Type.DictType _) = True
    isContainerType (Type.ListType _) = True
    isContainerType Type.TupleType {} = True
    isContainerType _ = False

-- | A fragment of the text representation of a signature, with information
-- for finding the spans of parameters inside of the text representation.
data SignatureFragment
  = -- | A plaintext fragment of a signature.
    Text !String
  | -- | A fragment that contains the text for a parameter of the signature. The
    -- parameter is identified by the 'ExprKey'.
    Param !ExprKey !String
  deriving (Eq, Show)

fragmentText :: SignatureFragment -> String
fragmentText (Text text) = text
fragmentText (Param _ text) = text

fragmentParam :: SignatureFragment -> Maybe ExprKey
fragmentParam (Text _) = Nothing
fragmentParam (Param key _) = Just key

instance IsString SignatureFragment where
  fromString string = Text string

-- | TODO
signatureToFragments :: Signature -> [SignatureFragment]
signatureToFragments signature = case signatureType signature of
  Type.DictType fieldMap
    | M.null fieldMap -> ["{}"]
    | otherwise ->
      let fields = intersperse ", " (map fieldToFragment (M.toList fieldMap))
       in concat [["{"], fields, ["}"]]
  Type.ListType inner -> ["[", showInnerType inner, "]"]
  Type.TupleType (a, b, cs) ->
    let fieldTypes = a : b : cs
        fields = intersperse ", " (map showInnerType fieldTypes)
     in concat [["("], fields, [")"]]
  typ -> [fromString (show typ)]
  where
    showInnerType :: IsString s => Type -> s
    showInnerType (Type.DictType _) = "{ ... }"
    showInnerType typ = fromString (show typ)

    fieldToFragment :: (String, Type.DictEntryType) -> SignatureFragment
    fieldToFragment (key, Type.DictRequired typ) =
      Param (Key key) $ printf "%s: %s" key (showInnerType typ :: String)
    fieldToFragment (key, Type.DictOptional typ) =
      Param (Key key) $ printf "%s?: %s" key (showInnerType typ :: String)

-- | TODO
showFragments :: [SignatureFragment] -> String
showFragments = concatMap fragmentText

-- | TODO
getParamsFromFragments :: [SignatureFragment] -> [LSP.ParameterInformation]
getParamsFromFragments fragments = map labelToInfo $ go 0 fragments
  where
    go :: LSP.UInt -> [SignatureFragment] -> [LSP.ParameterLabel]
    go _ [] = []
    go offset (fragment : remaining) = case getParamFromFragment offset fragment of
      (offset', Nothing) -> go offset' remaining
      (offset', Just param) -> param : go offset' remaining

    getParamFromFragment :: LSP.UInt -> SignatureFragment -> (LSP.UInt, Maybe LSP.ParameterLabel)
    getParamFromFragment offset (Text text) =
      (offset + fromIntegral (length text), Nothing)
    getParamFromFragment offset (Param _ text) =
      let end = offset + fromIntegral (length text)
       in (end, Just $ LSP.ParameterLabelOffset offset end)

    labelToInfo :: LSP.ParameterLabel -> LSP.ParameterInformation
    labelToInfo label =
      LSP.ParameterInformation
        { _label = label,
          _documentation = Nothing
        }

-- | TODO
findActiveParameterIndex :: [SignatureFragment] -> ExprKey -> Maybe LSP.UInt
findActiveParameterIndex fragments key =
  fromIntegral <$> elemIndex key (mapMaybe fragmentParam fragments)

-- | Lift a 'Maybe' into a 'MaybeT' monad transformer.
hoistMaybe :: Monad m => Maybe a -> MaybeT m a
hoistMaybe = MaybeT . pure
