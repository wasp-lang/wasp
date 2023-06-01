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
import Data.List (elemIndex, foldl', intersperse)
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
import Wasp.LSP.TypeInference (ExprKey (Key, List, Tuple), findExprPathAtLocation, findTypeForPath)

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
          logM "[getSignatureHelpAtPosition] no signature found"
          return emptyHelp
        Just signature -> do
          logM "[getSignatureHelpAtPosition] found a signature"
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

-- | 'Signature' describes the expected type at a specific location inside of
-- a container in a wasp file. Every signature includes the type of the container
-- surrounding the location. For example, the container type for a cursor
-- positioned inside of a dictionary would be the type of the dictionary.
--
-- When the location is at a more specific place inside of the container--for
-- example, immediately after @key: @ in a dictionary--then we say the location
-- is at a parameter, and the parameter is described in relation to the container
-- with an 'ExprKey'.
data Signature
  = -- | Inside a container, but not at any parameter inside of it.
    InContainer !Type
  | -- | Inside a container at the parameter specified by the 'ExprKey'. This
    -- is named \"param\" to be consistent with the naming conventions used by
    -- the LSP.
    AtParam !Type !ExprKey
  deriving (Eq, Show)

signatureType :: Signature -> Type
signatureType (InContainer typ) = typ
signatureType (AtParam typ _) = typ

signatureParam :: Signature -> Maybe ExprKey
signatureParam (InContainer _) = Nothing
signatureParam (AtParam _ key) = Just key

-- | @'findSignatureAtLocation' sourceCode location@ runs type inference at the
-- given location to try to find a 'Signature'.
--
-- To do this, two types are inferred: the /tip type/ and the /parent type/. The
-- tip type is the type expected exactly at the cursor location and the parent
-- type is the type of the parent node of the tip. For example, if the cursor is
-- at the 2nd entry of a tuple with type @(string, number)@, then the tip type
-- is @number@ and the parent type is that tuple type.
--
-- Then we check if the cursor is 'InContainer' or 'AtParam'. It is 'InContainer'
-- only when the tip type is a container or if there is no parent type.
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
--
-- This is used as an intermediate form between 'Signature' and the response
-- object that the LSP specifies.
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

-- | Convert the container type of a signature to a list of fragments.
--
-- To avoid unreadably long signatures, dictionary types inside of the container
-- type are written @{ ... }@.
signatureToFragments :: Signature -> [SignatureFragment]
signatureToFragments signature = case signatureType signature of
  Type.DictType fieldMap
    | M.null fieldMap -> ["{}"]
    | otherwise ->
      let fields = intersperse ", " (map fieldToFragment (M.toList fieldMap))
       in concat [["{"], fields, ["}"]]
  Type.ListType inner -> ["[", Param List (showInnerType inner), "]"]
  Type.TupleType (a, b, cs) ->
    let fieldTypes = a : b : cs
        fields = intersperse ", " (zipWith (\n -> Param (Tuple n) . showInnerType) [0 ..] fieldTypes)
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

-- | Convert a list of fragments to a string by concatenating the text of each
-- fragment.
showFragments :: [SignatureFragment] -> String
showFragments = concatMap fragmentText

-- | Search through a list of fragments to find the parameters.
--
-- The LSP wants the starting and ending offset of each parameter in the signature
-- help text (computed in 'showFragments'), so this function also has to keep
-- track of the 0-based offset into the final text.
getParamsFromFragments :: [SignatureFragment] -> [LSP.ParameterInformation]
getParamsFromFragments fragments =
  reverse $ map labelToInfo $ snd $ foldl' go (0, []) fragments
  where
    -- A left fold is used because offset needs to increase as you go right across
    -- the list. This means the parameter list needs to be reversed after the fold
    -- so that they are in the same order as the parameters in the fragments.
    go :: (LSP.UInt, [LSP.ParameterLabel]) -> SignatureFragment -> (LSP.UInt, [LSP.ParameterLabel])
    go (offset, labels) (Text text) = (offset + fromIntegral (length text), labels)
    go (offset, labels) (Param _ text) =
      let end = offset + fromIntegral (length text)
       in (end, LSP.ParameterLabelOffset offset end : labels)

    labelToInfo :: LSP.ParameterLabel -> LSP.ParameterInformation
    labelToInfo label =
      LSP.ParameterInformation
        { _label = label,
          _documentation = Nothing
        }

-- | Find the index of the active parameter in a list of fragments.
--
-- NOTE: This function computes the index into the parameter list, not the
-- fragment list. This is why plain 'Text' fragments are filtered out of the
-- list before indexing (via @mapMaybe fragmentParam@).
findActiveParameterIndex :: [SignatureFragment] -> ExprKey -> Maybe LSP.UInt
findActiveParameterIndex fragments key =
  fromIntegral <$> elemIndex key (mapMaybe fragmentParam fragments)

-- | Lift a 'Maybe' into a 'MaybeT' monad transformer.
hoistMaybe :: Monad m => Maybe a -> MaybeT m a
hoistMaybe = MaybeT . pure
