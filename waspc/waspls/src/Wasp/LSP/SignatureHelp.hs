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
import qualified Data.Text as Text
import qualified Language.LSP.Types as LSP
import Wasp.Analyzer.Parser.CST.Traverse (Traversal, fromSyntaxForest)
import Wasp.Analyzer.Type (Type)
import qualified Wasp.Analyzer.Type as Type
import Wasp.LSP.ServerState (ServerState, cst, currentWaspSource)
import Wasp.LSP.Syntax (lspPositionToOffset, toOffset)
import Wasp.LSP.TypeInference (ExprKey (Key, List, Tuple), findExprPathAtLocation, findTypeForPath)

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
      getSignatureContext src location >>= \case
        Nothing -> do
          logM "[getSignatureHelpAtPosition] no type hint to provide signature for"
          return emptyHelp
        Just (JustSignature typ) -> do
          logM "[getSignatureHelpAtPosition] type hint found; not at argument"
          let label = showSig $ sigShow typ
          return $
            LSP.SignatureHelp
              { _signatures =
                  LSP.List
                    [ LSP.SignatureInformation
                        { _parameters = Nothing,
                          _activeParameter = Nothing,
                          _label = Text.pack label,
                          _documentation = Nothing
                        }
                    ],
                _activeSignature = Just 0,
                _activeParameter = Nothing
              }
        Just (ArgSignature typ path _) -> do
          logM $ "[getSignatureHelpAtPosition] type hint found; at argument " ++ show path
          let shown = sigShow typ
          let label = showSig shown
          let params = sigParams shown
          let activeParam = sigParamIdx shown path
          logM $ "[getSignatureHelpAtPosition] at param idx = " ++ show activeParam ++ " params = " ++ show params
          return $
            LSP.SignatureHelp
              { _signatures =
                  LSP.List
                    [ LSP.SignatureInformation
                        { _parameters = Just $ LSP.List params,
                          _activeParameter = activeParam,
                          _label = Text.pack label,
                          _documentation = Nothing
                        }
                    ],
                _activeSignature = Just 0,
                _activeParameter = activeParam
              }
  where
    emptyHelp =
      LSP.SignatureHelp
        { _signatures = LSP.List [],
          _activeSignature = Nothing,
          _activeParameter = Nothing
        }

data SignatureShow = SignatureShow
  { sigKey :: !(Maybe ExprKey),
    sigText :: !String
  }
  deriving (Eq, Show)

sigShow :: Type -> [SignatureShow]
sigShow (Type.DictType fieldMap)
  | null fields = [SignatureShow Nothing "{}"]
  | otherwise =
    let preamble = [SignatureShow Nothing "{\n  "]
        sep = SignatureShow Nothing ",\n  "
        fieldsShown = intersperse sep (map showField fields)
        postamble = [SignatureShow Nothing "\n}"]
     in concat [preamble, fieldsShown, postamble]
  where
    fields = M.toList fieldMap
    showField :: (String, Type.DictEntryType) -> SignatureShow
    showField (k, Type.DictRequired v) =
      SignatureShow (Just $ Key k) $ k ++ ": " ++ showSig (sigShow v)
    showField (k, Type.DictOptional v) =
      SignatureShow (Just $ Key k) $ k ++ ": " ++ showSig (sigShow v) ++ "?"
sigShow (Type.ListType ty) =
  [ SignatureShow Nothing "[",
    SignatureShow (Just List) (show ty),
    SignatureShow Nothing "]"
  ]
sigShow (Type.TupleType (a, b, cs)) =
  let ts = a : b : cs
      sep = SignatureShow Nothing ", "
      tsShown = intersperse sep $ zipWith showT [0 ..] ts
   in SignatureShow Nothing "(" : tsShown ++ [SignatureShow Nothing ")"]
  where
    showT :: Int -> Type -> SignatureShow
    showT n t = SignatureShow (Just $ Tuple n) $ show t
sigShow ty = [SignatureShow Nothing $ show ty]

showSig :: [SignatureShow] -> String
showSig = concatMap sigText

sigParams :: [SignatureShow] -> [LSP.ParameterInformation]
sigParams allShows = map labelToInfo $ getParams 0 allShows
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

sigParamIdx :: [SignatureShow] -> ExprKey -> Maybe LSP.UInt
sigParamIdx allShows path = search 0 allShows
  where
    search :: LSP.UInt -> [SignatureShow] -> Maybe LSP.UInt
    search _ [] = Nothing
    -- NOTE: do not increment the index on non-param SignatureShows, because
    -- the index being returned is the index into the parameter list, not the
    -- show list
    search idx (SignatureShow Nothing _ : remaining) = search idx remaining
    search idx (SignatureShow (Just p) _ : remaining)
      | p == path = Just idx
      | otherwise = search (idx + 1) remaining

data SignatureContext
  = JustSignature !Type
  | ArgSignature !Type !ExprKey !Type
  deriving (Eq, Show)

getSignatureContext ::
  (MonadLog m) =>
  String ->
  Traversal ->
  m (Maybe SignatureContext)
getSignatureContext src t = runMaybeT $ do
  exprPath <- hoistMaybe $ findExprPathAtLocation src t
  lift $ logM $ "[SignatureHelp] at expr path " ++ show exprPath
  guard $ not $ null exprPath
  case exprPath of
    [path] -> JustSignature <$> hoistMaybe (findTypeForPath [path])
    path -> do
      -- Using init/last here is fine since we know it has at least 2 elements
      tipType <- hoistMaybe $ findTypeForPath path
      parentType <- hoistMaybe $ findTypeForPath $ init path
      if isContainerType tipType
        then return $ JustSignature tipType
        else return $ ArgSignature parentType (last path) tipType
  where
    isContainerType :: Type -> Bool
    isContainerType (Type.DictType _) = True
    isContainerType (Type.ListType _) = True
    isContainerType Type.TupleType {} = True
    isContainerType _ = False

hoistMaybe :: Monad m => Maybe a -> MaybeT m a
hoistMaybe = MaybeT . pure
