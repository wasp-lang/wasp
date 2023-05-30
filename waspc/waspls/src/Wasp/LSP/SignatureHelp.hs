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
import Data.List (intersperse)
import Data.Maybe (fromMaybe)
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
      return emptyHelp
    Just syntax -> do
      let offset = lspPositionToOffset src position
      let location = toOffset offset (fromSyntaxForest syntax)
      findSignatureContext src location >>= \case
        Nothing -> do
          logM "[getSignatureHelpAtPosition] no type hint to provide signature for"
          return emptyHelp
        Just signatureContext -> do
          logM "[getSignatureHelpAtPosition] type hint found"
          let shown = showSignatureType $ signatureContextType signatureContext
          let label = showSS shown
          let params = extractParametersFromSignature shown
          let activeParam = signatureContextKey signatureContext >>= findParameterIdxForKey shown
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
                          _label = Text.pack label,
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

-- | Describes the kind of place a signature is being created for.
data SignatureContext
  = -- | Inside a container, but not inside any param of the container.
    Container !Type
  | -- | Inside a parameter of a container. For example, in the value associated
    -- with a key of a dictionary.
    Param !Type !ExprKey
  deriving (Eq, Show)

signatureContextType :: SignatureContext -> Type
signatureContextType (Container typ) = typ
signatureContextType (Param typ _) = typ

signatureContextKey :: SignatureContext -> Maybe ExprKey
signatureContextKey (Container _) = Nothing
signatureContextKey (Param _ key) = Just key

findSignatureContext ::
  (MonadLog m) =>
  String ->
  Traversal ->
  m (Maybe SignatureContext)
findSignatureContext src t = runMaybeT $ do
  exprPath <- hoistMaybe $ findExprPathAtLocation src t
  lift $ logM $ "[SignatureHelp] at expr path " ++ show exprPath
  guard $ not $ null exprPath
  case exprPath of
    [path] -> Container <$> hoistMaybe (findTypeForPath [path])
    path -> do
      -- Using init/last here is fine since we know it has at least 2 elements.
      tipType <- hoistMaybe $ findTypeForPath path
      parentType <- hoistMaybe $ findTypeForPath $ init path
      if isContainerType tipType
        then return $ Container tipType
        else return $ Param parentType (last path)
  where
    isContainerType :: Type -> Bool
    isContainerType (Type.DictType _) = True
    isContainerType (Type.ListType _) = True
    isContainerType Type.TupleType {} = True
    isContainerType _ = False

-- | A piece of the text representation of a signature, with information
-- for finding the spans of parameters inside of the text representation.
data SignatureShow = SignatureShow
  { ssKey :: !(Maybe ExprKey),
    ssText :: !String
  }
  deriving (Eq, Show)

showSignatureType :: Type -> [SignatureShow]
showSignatureType (Type.DictType fieldMap)
  | null fields = [SignatureShow Nothing "{}"]
  | otherwise =
    let openDict = [SignatureShow Nothing "{"]
        sep = SignatureShow Nothing ", "
        fieldsShown = intersperse sep (map showField fields)
        closeDict = [SignatureShow Nothing "}"]
     in concat [openDict, fieldsShown, closeDict]
  where
    fields = M.toList fieldMap
    showField :: (String, Type.DictEntryType) -> SignatureShow
    showField (k, Type.DictRequired v) =
      SignatureShow (Just $ Key k) $ printf "%s: %s" k $ showInnerType v
    showField (k, Type.DictOptional v) =
      SignatureShow (Just $ Key k) $ printf "%s?: %s" k $ showInnerType v
showSignatureType (Type.ListType ty) =
  [ SignatureShow Nothing "[",
    SignatureShow (Just List) (showInnerType ty),
    SignatureShow Nothing "]"
  ]
showSignatureType (Type.TupleType (a, b, cs)) =
  let ts = a : b : cs
      sep = SignatureShow Nothing ", "
      tsShown = intersperse sep $ zipWith showT [0 ..] ts
   in SignatureShow Nothing "(" : tsShown ++ [SignatureShow Nothing ")"]
  where
    showT :: Int -> Type -> SignatureShow
    showT n t = SignatureShow (Just $ Tuple n) $ showInnerType t
showSignatureType ty = [SignatureShow Nothing $ show ty]

showInnerType :: Type -> String
showInnerType (Type.DictType _) = "{ ... }"
showInnerType ty = show ty

showSS :: [SignatureShow] -> String
showSS = concatMap ssText

extractParametersFromSignature :: [SignatureShow] -> [LSP.ParameterInformation]
extractParametersFromSignature allShows = map labelToInfo $ getParams 0 allShows
  where
    getParams :: LSP.UInt -> [SignatureShow] -> [LSP.ParameterLabel]
    getParams _ [] = []
    getParams offset (s : ss) = case getParam offset s of
      (offset', Nothing) -> getParams offset' ss
      (offset', Just p) -> p : getParams offset' ss

    getParam :: LSP.UInt -> SignatureShow -> (LSP.UInt, Maybe LSP.ParameterLabel)
    getParam offset (SignatureShow Nothing txt) =
      (offset + fromIntegral (length txt), Nothing)
    getParam offset (SignatureShow (Just _) txt) =
      let end = offset + fromIntegral (length txt)
       in (end, Just $ LSP.ParameterLabelOffset offset end)

    labelToInfo :: LSP.ParameterLabel -> LSP.ParameterInformation
    labelToInfo label =
      LSP.ParameterInformation
        { _label = label,
          _documentation = Nothing
        }

findParameterIdxForKey :: [SignatureShow] -> ExprKey -> Maybe LSP.UInt
findParameterIdxForKey allShows path = search 0 allShows
  where
    search :: LSP.UInt -> [SignatureShow] -> Maybe LSP.UInt
    search _ [] = Nothing
    -- NOTE: do not increment the index on non-param SignatureShows, because
    -- the index being returned is the index into the parameter list, not the
    -- show list.
    search idx (SignatureShow Nothing _ : remaining) = search idx remaining
    search idx (SignatureShow (Just p) _ : remaining)
      | p == path = Just idx
      | otherwise = search (idx + 1) remaining

hoistMaybe :: Monad m => Maybe a -> MaybeT m a
hoistMaybe = MaybeT . pure
