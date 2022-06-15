{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Wasp.Backend.ConcreteParser.Internal
  ( -- * Internal parsing library
    Parser,
    parse,

    -- * Combinators
    (-->),
    as,
    group,
    perhaps,
    -- | `(<>)` is used to add choice to a parser.
    --
    -- __Example:__
    --
    -- >>> let number = int <> double
    -- >>> parse [T.Token T.Int 2] number
    -- (...,[],[SyntaxNode (Token T.Int) 2 []])
    -- >>> parse [T.Token  T.Double 3] number
    -- (...,[],[SyntaxNode (Token T.Double) 2 []])
    (<>),

    -- * Primitive parsers

    -- | These parsers (besides `eof`) consume a single token of the kind
    -- matching their name. The syntax node they produce has an "S.Token" kind,
    -- unless otherwise specified.
    lparen,
    rparen,
    lsquare,
    rsquare,
    lcurly,
    rcurly,
    comma,
    colon,
    kwImport,
    kwFrom,
    kwTrue,
    kwFalse,
    string,
    int,
    double,
    lquote,
    rquote,
    quoted,
    -- | A primitive for "T.Identifier" is not included: this should always be
    -- done with an `as`, i.e. @"T.Identifier" ``as`` "S.DictKey"@
    eof,

    -- * Future improvements

    -- | While this works well in many cases, it can do what feels like the
    -- wrong thing in some cases. For example, consider the following invalid
    -- wasp code:
    --
    -- @
    -- route TestRoute { path: "/", to:
    --
    -- page TestPage { fn: import TestPage from "@ext/TestPage.js" }
    -- @
    --
    -- To a human, this should parse as two declaration statements. But the
    -- current system instead considers the first occurence of @TestPage@ as
    -- a dictionary key, then complains about the following left curly brace.
    --
    -- This is due to the unwinding behavior on errors being very short-sighted:
    -- it only cares whether the next non-white token will succeed.
    --
    -- A possible fix to this is to explore all possible (or many possible)
    -- unwinding stopping points and figuring out which one results in the
    -- fewest future parse errors.
    --
    -- TODO: Create an issue report for this when merged into main
  )
where

import Control.Monad (when)
import Control.Monad.Except (ExceptT, MonadError (catchError, throwError), runExceptT)
import Control.Monad.State.Strict (State, gets, modify, runState)
import Control.Monad.Writer.Strict (MonadWriter (tell), WriterT, runWriterT)
import Wasp.Backend.ConcreteSyntax (SyntaxKind, SyntaxNode (SyntaxNode, snodeChildren, snodeKind, snodeWidth))
import qualified Wasp.Backend.ConcreteSyntax as S
import Wasp.Backend.ParseError
import Wasp.Backend.Token (Token (tokenKind, tokenWidth), TokenKind, tokenKindIsTrivia)
import qualified Wasp.Backend.Token as T

-- | A monoidal parser type. Use combinators and primitives to build more
-- complex parsers. Run with `parse`, which supports reasonable automatic error
-- recovery.
data Parser
  = -- | Always succeeds, consuming no input
    Succeed
  | -- | Consumes and produces a node when the next token is the specified kind.
    -- Also consumes and produces any whitespace (even on failure).
    --
    -- On failure, does not consume any input (except whitespace).
    Primitive TokenKind
  | -- | Succeeds at the end of a file
    EOF
  | -- | Run the inner parser and place its resulting node in a node labelled by
    -- the given kind. If it fails, try recovering:
    --
    -- 1. If the failed token is in top `pstateFollowing`, then produce an error
    -- but do not consume the token.
    --
    -- 2. Otherwise, produce an error and consume the token.
    Group SyntaxKind Parser
  | -- | @"As" skind tkind@ is like @"Primitive" tkind@, except the resulting
    -- syntax node is labeled by @skind@ instead of @tkind@.
    As SyntaxKind TokenKind
  | -- | Run the first parser, then the second parser (aborts early if the first
    -- parser fails
    Chain Parser Parser
  | -- | Tries the left parser first. If it fails, backtrack and try the right
    -- parser
    Alternative Parser Parser
  deriving (Eq, Ord, Show)

instance Semigroup Parser where
  x <> y = Alternative x y

instance Monoid Parser where
  mempty = Succeed

-- | Run a "Parser" on an input of tokens. Returns a tuple @(logs, errors, nodes)@.
--
-- @logs@ can be safely ignored, they are for debugging purposes and will be
-- removed once this parsing library is more stable.
parse :: [Token] -> Parser -> ([String], [ParseError], [SyntaxNode])
parse tokens parser =
  let initialState =
        ParseState
          { pstateInput = tokens,
            pstateNodes = [],
            pstateErrors = [],
            pstateNextOffset = 0,
            pstateLastOffset = 0,
            pstateFollowing = []
          }
   in case runState (runWriterT (runExceptT (doParse parser))) initialState of
        ((Left err, logs), state) -> case err of
          Unwind _ -> error "Unwind at top-level Parser (impossible, this is a bug in Wasp.Backend.ConcreteParser.Internal)"
          ParseError perr ->
            let errs = pstateErrors state ++ [perr]
                newNode = errorNode perr False
                nodes = pstateNodes state ++ [newNode]
             in (logs, errs, nodes)
        ((_, logs), state) -> (logs, pstateErrors state, pstateNodes state)

data ParseState = ParseState
  { -- | Remaining tokens in input
    pstateInput :: [Token],
    -- | Nodes collected. Reset every time a group is entered/exited (sorta)
    pstateNodes :: [SyntaxNode],
    -- | Errors collected, to be reported
    pstateErrors :: [ParseError],
    -- | 0-based offset of the start of the next token (@head . `pstateInput`@)
    pstateNextOffset :: !Int,
    -- | 0-based offset of the start of the last consumed token
    pstateLastOffset :: !Int,
    -- | Stack of possible first tokens, collected when we enter a group as the
    -- first parser in a "Chain".
    --
    -- A member @None@ represents EOF.
    pstateFollowing :: [ExpectedSet]
  }
  deriving (Eq, Show, Ord)

data ParseException = Unwind (Maybe TokenKind) | ParseError ParseError

-- | Parsing monad
type ParserM a = ExceptT ParseException (WriterT [String] (State ParseState)) a

-- | Internal function to run a "Parser" in the "ParserM" monad.
doParse :: Parser -> ParserM ()
doParse Succeed = consumeWhitespace
doParse (Primitive kind) =
  (consumeWhitespace >> peek) >>= \case
    Just nextToken | tokenKind nextToken == kind -> do
      advance
      let newNode =
            SyntaxNode
              { snodeKind = S.Token kind,
                snodeWidth = tokenWidth nextToken,
                snodeChildren = []
              }
      pushNode newNode
    nextToken -> makeError nextToken (esetSingleton (Just kind))
doParse EOF =
  (consumeWhitespace >> peek) >>= \case
    Just nextToken -> do
      startOffset <- gets pstateNextOffset
      advance
      let firstTokenKind = tokenKind nextToken
      -- TODO: consider reporting all kinds here? not sure
      _tokenKinds <- collectUntilEOF
      endOffset <- gets pstateNextOffset
      throwError $ ParseError $ Unexpected (Span startOffset endOffset) firstTokenKind (esetSingleton Nothing)
      where
        collectUntilEOF :: ParserM [TokenKind]
        collectUntilEOF =
          peek >>= \case
            Just next -> advance >> ((tokenKind next :) <$> collectUntilEOF)
            Nothing -> pure []
    Nothing -> do
      return ()
doParse (Group label inner) = do
  let pushGroupNode children =
        let width = sum $ map snodeWidth children
         in pushNode $
              SyntaxNode
                { snodeKind = label,
                  snodeWidth = width,
                  snodeChildren = children
                }

  debug $ "(Group) Entering group " ++ show label
  consumeWhitespace
  siblingNodes <- gets pstateNodes
  modify (\s -> s {pstateNodes = []})
  result <- tryError $ doParse inner
  childNodes <- gets pstateNodes
  modify (\s -> s {pstateNodes = siblingNodes})
  case result of
    Nothing -> do
      pushGroupNode childNodes
      debug $ "(Group) Exiting group " ++ show label
    Just err -> do
      case err of
        Unwind kind -> do
          -- When unwinding, check if this is the level where we have the
          -- following token. If it is, then we can get rid of the unwind.
          --
          -- Otherwise, we re-throw the unwind

          followingStack <- gets pstateFollowing
          let errKindIsImmediatelyFollowing = case followingStack of
                [] -> False -- technically unreachable, but this is sensible (TODO: consider using error here?)
                (following : _) -> kind `esetMember` following

          pushGroupNode childNodes

          if errKindIsImmediatelyFollowing
            then debug $ "(Group) Exiting group " ++ show label
            else debug ("(Group) Unwinding from group " ++ show label) >> throwError (Unwind kind)
        ParseError perr -> do
          -- ASSUMPTION: The token errored on has not been consumed
          --
          -- First, check whether the kind we errored on is in the first `pstateFollowing`
          -- list. If it is not, we consume the token.
          --
          -- Then, report the error to the state. Now, we have recovered!
          followingStack <- gets pstateFollowing
          let errKind = case perr of
                UnexpectedEOF _ _ -> Nothing
                Unexpected _ k _ -> Just k
          let errKindIsFollowingKind = any (errKind `esetMember`) followingStack
          let errKindIsImmediatelyFollowing = case followingStack of
                [] -> False -- technically unreachable, but this is sensible (TODO: consider using error here?)
                (following : _) -> errKind `esetMember` following

          -- Make the error node and possibly consume the token
          nodeForErr <-
            if errKindIsFollowingKind
              then do
                return $ errorNode perr True
              else do
                advance
                return $ errorNode perr False

          -- Report the error and add the error node
          pushError perr
          pushGroupNode $ childNodes ++ [nodeForErr]

          when (errKindIsFollowingKind && not errKindIsImmediatelyFollowing) $ do
            debug $ "(Group) Starting unwind from group " ++ show label
            throwError $ Unwind errKind

          debug $ "(Group) Exiting group " ++ show label
doParse (As label kind) =
  (consumeWhitespace >> peek) >>= \case
    Just nextToken | tokenKind nextToken == kind -> do
      advance
      let newNode =
            SyntaxNode
              { snodeKind = label,
                snodeWidth = tokenWidth nextToken,
                snodeChildren = []
              }
      pushNode newNode
    nextToken -> makeError nextToken (esetSingleton (Just kind))
doParse (Chain first second) = do
  consumeWhitespace
  -- First, figure out following set for @second@
  let following = findFirstTokens second
  pushFollowing following
  mbError <- tryError (doParse first)
  -- Make sure to remove following set now that we're leaving this "Then"
  popFollowing
  -- Run second (or throw error if first failed)
  case mbError of
    Nothing -> doParse second
    Just err -> throwError err
doParse (Alternative left right) = do
  consumeWhitespace
  let leftFirstTokens = findFirstTokens left
  let rightFirstTokens = findFirstTokens right
  nextToken <- peek
  let nextKind = tokenKind <$> nextToken
  if nextKind `willSucceedIn` left
    then doParse left
    else
      if nextKind `willSucceedIn` right
        then doParse right
        else makeError nextToken (leftFirstTokens `esetUnion` rightFirstTokens)

-- | Advance past whitespace, making nodes for the whitespace as it goes
consumeWhitespace :: ParserM ()
consumeWhitespace =
  peek >>= \case
    Nothing -> pure ()
    Just nextToken
      -- Consume any whitespace
      | tokenKindIsTrivia (tokenKind nextToken) -> do
        advance
        let newNode =
              SyntaxNode
                { snodeKind = S.Token (tokenKind nextToken),
                  snodeWidth = tokenWidth nextToken,
                  snodeChildren = []
                }
        pushNode newNode
        consumeWhitespace
      | otherwise -> pure ()

-- | Peek the immediate next token in input (including whitespace)
peek :: ParserM (Maybe Token)
peek =
  gets pstateInput >>= \case
    [] -> return Nothing
    tok : _ -> debug ("(peek) " ++ show tok) >> return (Just tok)

-- | Advance past the next token in the input, updating offsets.
advance :: ParserM ()
advance =
  gets pstateInput >>= \case
    [] -> modify (\s -> s {pstateLastOffset = pstateNextOffset s})
    (consumed : remaining) -> do
      newLastOffset <- gets pstateNextOffset
      let newNextOffset = newLastOffset + tokenWidth consumed
      debug $ "(advance) " ++ show consumed ++ ", newLastOffset=" ++ show newLastOffset ++ ", newNextOffset=" ++ show newNextOffset
      modify (\s -> s {pstateInput = remaining, pstateLastOffset = newLastOffset, pstateNextOffset = newNextOffset})

-- | @makeError actual expected@ adds an error node and throws an error.
--
-- Assumes @actual@ has not been `advance`d past yet (and does not advance past
-- it).
makeError :: Maybe Token -> ExpectedSet -> ParserM ()
makeError Nothing eset = do
  offset <- gets pstateNextOffset
  throwError $ ParseError $ UnexpectedEOF offset eset
makeError (Just actual) eset = do
  offset <- gets pstateNextOffset
  throwError $ ParseError $ Unexpected (Span offset (offset + tokenWidth actual)) (tokenKind actual) eset

-- | Make an error node. Error node should only be produced when catching and
-- pushing errors to the state.
errorNode ::
  -- | error to make an error node from
  ParseError ->
  -- | If "True", give the error node a width of 0. Otherwise, use the
  -- width of the error item.
  Bool ->
  SyntaxNode
errorNode (UnexpectedEOF _ _) _ =
  SyntaxNode
    { snodeKind = S.Error,
      snodeWidth = 0,
      snodeChildren = []
    }
errorNode (Unexpected (Span so eo) _ _) zeroWidth =
  SyntaxNode
    { snodeKind = S.Error,
      snodeWidth = if zeroWidth then 0 else eo - so,
      snodeChildren = []
    }

-- | Add a node to the output.
pushNode :: SyntaxNode -> ParserM ()
pushNode node = modify (\s -> s {pstateNodes = pstateNodes s ++ [node]})

-- | Add an error to the output (does not throw the error).
pushError :: ParseError -> ParserM ()
pushError err = modify (\s -> s {pstateErrors = pstateErrors s ++ [err]})

-- | Add an "ExpectedSet" to the following stack.
pushFollowing :: ExpectedSet -> ParserM ()
pushFollowing eset = modify (\s -> s {pstateFollowing = eset : pstateFollowing s})

-- | Remove the last added "ExpectedSet" from the following stack.
popFollowing :: ParserM ()
popFollowing = modify (\s -> s {pstateFollowing = drop 1 $ pstateFollowing s})

-- | Check if a "TokenKind" (or end of file) is one of the first possible tokens
-- in a "Parser".
willSucceedIn :: Maybe TokenKind -> Parser -> Bool
_ `willSucceedIn` Succeed = True
k `willSucceedIn` parser = k `esetMember` findFirstTokens parser

-- | Find the "ExpectedSet" of "TokenKind"s that the "Parser" would succeed on
-- as its first input.
findFirstTokens :: Parser -> ExpectedSet
findFirstTokens parser = fst $ go parser esetEmpty
  where
    -- Returns (firstTokens, can succeed with no input?)
    go :: Parser -> ExpectedSet -> (ExpectedSet, Bool)
    go Succeed eset = (eset, True)
    go (Primitive kind) eset = (kind `esetInsertKind` eset, False)
    go EOF eset = (Nothing `esetInsert` eset, False)
    go (Group _ p) eset = go p eset
    go (As _ kind) eset = (kind `esetInsertKind` eset, False)
    go (Chain first second) eset =
      let (eset', firstCanBeEmpty) = go first eset
       in if firstCanBeEmpty
            then go second eset'
            else (eset', False)
    go (Alternative left right) eset =
      let (eset', leftCanBeEmpty) = go left eset
          (eset'', rightCanBeEmpty) = go right eset'
       in (eset'', leftCanBeEmpty || rightCanBeEmpty)

-- | Add a debug message to the output.
debug :: String -> ParserM ()
debug msg = tell [msg]

-- Combinators

infixr 9 -->

-- | Run parsers one after another.
--
-- __Example:__
--
-- >>> kwTrue --> kwFalse
-- -- Results in a parser that will match inputs like "true false"
(-->) :: Parser -> Parser -> Parser
first --> second = Chain first second

infix 9 `as`

-- | Matches a single token of the specified "TokenKind" and produce an output
-- node with the given "SyntaxKind".
--
-- __Example:__
--
-- >>> parse [T.Token T.Identifier 3] $ T.Identifier `as` S.DeclName
-- (...,[],[SyntaxNode {snodeKind = DeclName, snodeWidth = 3, snodeChildren = []}])
as :: TokenKind -> SyntaxKind -> Parser
tkind `as` skind = As skind tkind

-- | Runs a parser and places the nodes that parser adds to the output in a
-- parent node.
--
-- __Example:__
--
-- >>> parse [T.Token T.Identifier 3, T.Token T.Int 2] $ group S.Decl (identifier --> int)
-- (...,[],[SyntaxNode {snodeKind = Decl, snodeWidth = 5, snodeChildren = [SyntaxNode {snodeKind = Token Identifier, snodeWidth = 3, snodeChildren = []},SyntaxNode {snodeKind = Token Int, snodeWidth = 2, snodeChildren = []}]}])
group :: SyntaxKind -> Parser -> Parser
group kind parser = Group kind parser

-- | Run a parser or consume no input.
--
-- __Example:__
--
-- >>> parse [T.Token T.Int 1, T.Token T.Int 1] $ int --> perhaps comma --> int
-- (...,[],[SyntaxNode {snodeKind = Token Int, snodeWidth = 1, snodeChildren = []},SyntaxNode {snodeKind = Token Int, snodeWidth = 1, snodeChildren = []}])
perhaps :: Parser -> Parser
perhaps parser = parser <> Succeed

-- Token parsers

lparen :: Parser
lparen = Primitive T.LParen

rparen :: Parser
rparen = Primitive T.RParen

lsquare :: Parser
lsquare = Primitive T.LSquare

rsquare :: Parser
rsquare = Primitive T.RSquare

lcurly :: Parser
lcurly = Primitive T.LCurly

rcurly :: Parser
rcurly = Primitive T.RCurly

comma :: Parser
comma = Primitive T.Comma

colon :: Parser
colon = Primitive T.Colon

kwImport :: Parser
kwImport = Primitive T.KwImport

kwFrom :: Parser
kwFrom = Primitive T.KwFrom

-- | Produces a "S.BoolTrue" syntax node
kwTrue :: Parser
kwTrue = T.KwTrue `as` S.BoolTrue

-- | Produces a "S.BoolFalse" syntax node
kwFalse :: Parser
kwFalse = T.KwFalse `as` S.BoolFalse

-- | Produces a "S.String" syntax node
string :: Parser
string = T.String `as` S.String

-- | Produces a "S.Int" syntax node
int :: Parser
int = T.Int `as` S.Int

-- | Produces a "S.Double" syntax node
double :: Parser
double = T.Double `as` S.Double

lquote :: Parser
lquote = Primitive T.LQuote

rquote :: Parser
rquote = Primitive T.RQuote

quoted :: Parser
quoted = Primitive T.Quoted

-- | Succeeds when the parser is at the end of the file.
eof :: Parser
eof = EOF

-- UTILITIES:

tryError :: MonadError e m => m () -> m (Maybe e)
tryError m = (m >> return Nothing) `catchError` (return . Just)
