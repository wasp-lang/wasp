module Wasp.LSP.SignatureHelp
  ( getSignatureHelpAtPosition,
    signatureHelpTriggerCharacters,
    signatureHelpRetriggerCharacters,
  )
where

import Control.Applicative ((<|>))
import Control.Lens ((^.))
import Control.Monad (guard)
import Control.Monad.Log.Class (MonadLog, logM)
import Control.Monad.State.Class (MonadState, gets)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT (runMaybeT))
import qualified Data.HashMap.Strict as M
import Data.List (elemIndex, foldl', intersperse)
import Data.Maybe (mapMaybe)
import Data.String (IsString (fromString))
import qualified Data.Text as Text
import qualified Language.LSP.Types as LSP
import Text.Printf (printf)
import Wasp.Analyzer.Parser.CST.Traverse (Traversal, fromSyntaxForest)
import Wasp.Analyzer.Type (Type)
import qualified Wasp.Analyzer.Type as Type
import Wasp.LSP.ServerState (ServerState, cst, currentWaspSource)
import Wasp.LSP.Syntax (locationAtOffset, lspPositionToOffset)
import Wasp.LSP.TypeInference (ExprPathStep (DictKey, List, Tuple), findExprPathToLocation, findTypeForPath)
import Wasp.LSP.Util (hoistMaybe)

-- | Configuration for 'LSP.Options', used in "Wasp.LSP.Server".
--
-- When the client types one of these characters, it will send a SignatureHelp
-- request. It is configured so that signatures will display when starting a
-- dictionary, list, tuple, or starting a new field/value in the container.
signatureHelpTriggerCharacters :: Maybe [Char]
signatureHelpTriggerCharacters = Just "{[(:,"

-- | Configuration for 'LSP.Options', used in "Wasp.LSP.Server".
--
-- When a client is already displaying signature help, typing one of these
-- characters will cause it to update the displayed signature by sending a new
-- SignatureHelp request. It is configured so that signatures will update when
-- ending a dictionary, list, or  tuple.
--
-- NOTE: 'signatureHelpTriggerCharacters' are also counted as retrigger characters.
signatureHelpRetriggerCharacters :: Maybe [Char]
signatureHelpRetriggerCharacters = Just "}])"

-- | Get 'LSP.SignatureHelp' at a position in the wasp file. The signature
-- displays the type of the "container" that the position is in, if any.
--
-- SignatureHelp is usually displayed as a popup near the text you are typing,
-- and highlights the part of the signature you are writing.
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
    Just syntax ->
      let offset = lspPositionToOffset src position
          location = locationAtOffset offset (fromSyntaxForest syntax)
       in getSignatureHelpAtLocation src location
  where
    getSignatureHelpAtLocation src location =
      findSignatureAtLocation src location >>= \case
        Nothing -> do
          logM "[getSignatureHelpAtPosition] no signature found"
          return emptyHelp
        Just signature -> do
          logM "[getSignatureHelpAtPosition] found a signature"
          getLspSignatureFromSignature signature

    getLspSignatureFromSignature signature = do
      let signatureFragments = signatureToFragments signature
      let params = getLspParamsInfoFromFragments signatureFragments
      let activeParam = signatureParam signature >>= findActiveParameterIndex signatureFragments
      logM $ "[getSignatureHelpAtPosition] at param idx = " ++ show activeParam ++ " params = " ++ show params
      let signatureInformation =
            LSP.SignatureInformation
              { _label = Text.pack $ showFragments signatureFragments,
                _parameters = Just $ LSP.List params,
                -- NOTE: VSCode highlights the 0th parameter if activeParameter, so
                -- it's set to -1 when we don't want any parameter highlighted.
                _activeParameter = activeParam <|> Just (-1),
                _documentation = Nothing
              }
      return $
        LSP.SignatureHelp
          { _signatures = LSP.List [signatureInformation],
            _activeSignature = Just 0,
            _activeParameter = Nothing
          }

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
-- with an 'ExprPathStep'.
data Signature = Signature
  { signatureType :: !Type,
    signatureParam :: !(Maybe ExprPathStep)
  }
  deriving (Eq, Show)

-- | @'findSignatureAtLocation' sourceCode location@ runs type inference at the
-- given location to try to find a 'Signature'.
--
-- To do this, two types are inferred: the /location type/ and the
-- /container type/. The location type is the type expected exactly at the given
-- location and the container type is the type of that contains the location
-- type. For example, if the cursor is at the 2nd entry of a tuple with type
-- @(string, number)@, then the tip type is @number@ and the parent type is
-- that tuple type.
--
-- Then we check if the cursor is just inside a container or at a parameter
-- inside a container. It's in a container when the location type is a container
-- or if there is no container type.
findSignatureAtLocation ::
  (MonadLog m) =>
  String ->
  Traversal ->
  m (Maybe Signature)
findSignatureAtLocation src location = runMaybeT $ do
  exprPath <- hoistMaybe $ findExprPathToLocation src location
  lift $ logM $ "[SignatureHelp] at expr path " ++ show exprPath
  guard $ not $ null exprPath
  case exprPath of
    [path] -> do
      containerType <- hoistMaybe (findTypeForPath [path])
      return $ Signature containerType Nothing
    path -> do
      -- Using init/last here is OK since we know @path@ has at least 2 elements.
      locationType <- hoistMaybe $ findTypeForPath path
      if isContainerType locationType
        then return $ Signature locationType Nothing
        else do
          containerType <- hoistMaybe $ findTypeForPath $ init path
          return $ Signature containerType (Just $ last path)
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
-- format that the LSP specifies.
data SignatureFragment
  = -- | A plaintext fragment of a signature.
    PlaintextFragment !String
  | -- | A fragment that contains the text for a parameter of the signature. The
    -- parameter is identified by the 'ExprPathStep'.
    ParamFragment !ExprPathStep !String
  deriving (Eq, Show)

fragmentText :: SignatureFragment -> String
fragmentText (PlaintextFragment text) = text
fragmentText (ParamFragment _ text) = text

fragmentParam :: SignatureFragment -> Maybe ExprPathStep
fragmentParam (PlaintextFragment _) = Nothing
fragmentParam (ParamFragment key _) = Just key

instance IsString SignatureFragment where
  fromString string = PlaintextFragment string

-- | Convert the container type of a signature to a list of fragments.
--
-- To avoid unreadably long signatures, dictionary types inside of the container
-- type are written as @{ ... }@.
signatureToFragments :: Signature -> [SignatureFragment]
signatureToFragments signature = case signatureType signature of
  Type.DictType fieldMap
    | M.null fieldMap -> ["{}"]
    | otherwise ->
        let fields = intersperse ",\n  " (map fieldToFragment (M.toList fieldMap))
         in concat [["{\n  "], fields, ["\n}"]]
  Type.ListType inner -> ["[", ParamFragment List (showInnerType inner), "]"]
  Type.TupleType (a, b, cs) ->
    let fieldTypes = a : b : cs
        fields = intersperse ", " (zipWith (\n -> ParamFragment (Tuple n) . showInnerType) [0 ..] fieldTypes)
     in concat [["("], fields, [")"]]
  typ -> [fromString (show typ)]
  where
    showInnerType :: IsString s => Type -> s
    showInnerType (Type.DictType _) = "{ ... }"
    showInnerType typ = fromString (show typ)

    fieldToFragment :: (String, Type.DictEntryType) -> SignatureFragment
    fieldToFragment (key, Type.DictRequired typ) =
      ParamFragment (DictKey key) $ printf "%s: %s" key (showInnerType typ :: String)
    fieldToFragment (key, Type.DictOptional typ) =
      ParamFragment (DictKey key) $ printf "%s?: %s" key (showInnerType typ :: String)

-- | Convert a list of fragments to a string by concatenating the text of each
-- fragment.
showFragments :: [SignatureFragment] -> String
showFragments = concatMap fragmentText

-- | Search through a list of fragments to find the parameters.
--
-- The LSP wants the starting and ending offset of each parameter in the signature
-- help text (computed in 'showFragments'), so this function also has to keep
-- track of the 0-based offset into the final text.
getLspParamsInfoFromFragments :: [SignatureFragment] -> [LSP.ParameterInformation]
getLspParamsInfoFromFragments fragments =
  reverse $ map labelToInfo $ snd $ foldl' go (0, []) fragments
  where
    -- A left fold is used because offset needs to increase as you go right across
    -- the list. This means the parameter list needs to be reversed after the fold
    -- so that they are in the same order as the parameters in the fragments.
    go :: (LSP.UInt, [LSP.ParameterLabel]) -> SignatureFragment -> (LSP.UInt, [LSP.ParameterLabel])
    go (offset, labels) (PlaintextFragment text) = (offset + fromIntegral (length text), labels)
    go (offset, labels) (ParamFragment _ text) =
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
-- fragment list. This is why 'PlaintextFragment's are filtered out of the
-- list before indexing (via @mapMaybe fragmentParam@).
findActiveParameterIndex :: [SignatureFragment] -> ExprPathStep -> Maybe LSP.UInt
findActiveParameterIndex fragments key =
  fromIntegral <$> elemIndex key (mapMaybe fragmentParam fragments)
