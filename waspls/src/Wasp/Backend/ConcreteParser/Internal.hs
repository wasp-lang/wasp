{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Wasp.Backend.ConcreteParser.Internal
  ( -- * Internal parsing library
    GrammarRule,
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

    -- * Primitive grammar rules

    -- | These grammar rules (besides `eof`) consume a single token of the kind
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

    -- * Error Recovery

    -- | This parser has built-in automatic error recovery. Currently, error
    -- recovery occurs when an error is produced inside of a 'group' rule. Recovery
    -- is based on the possible tokens that can follow the current rule.
    --
    -- At a basic level, we can describe the mechanism as follows:
    --
    -- For a "GrammarRule" @'group' label first '-->' next@, if an error occurs
    -- while parsing @first@, check if the next token is in the "follow set" of
    -- the group (the follow set of a rule is the set of tokens that can appear
    -- directly after the rule in valid source). If it is, register an error in
    -- the output and do not consume the token. Otherwise, register an error and
    -- consume the token.
    --
    -- As an example, consider the grammar rules:
    --
    -- @
    -- stmts = eof <> (stmt --> stmts)
    -- stmt = group Stmt $ "let" --> identifier --> "=" --> expr --> ";"
    -- @
    --
    -- With the example source text:
    --
    -- @
    -- let x = 2
    -- let y = 4;
    -- @
    --
    -- A parse error will occur at the beginning of line 2, when @let@ is found
    -- instead of the expected @;@. The follow set of this @Stmt@ group is
    -- @[eof, "let"]@. Since the next token is in this set, it does not get
    -- consumed by the group. This leaves it available for the recursive call to
    -- @stmt@, which can parse successfully because of this, resulting in the
    -- following parse tree and error:
    --
    -- @
    -- [ Stmt ["let", "x", "=", "2", error]
    -- , Stmt ["let", "y", "=", "4", ";"]
    -- ]
    --
    -- [Unexpected "let" at line 2 column 1: expected ";"]
    -- @
    --
    -- However, this does not work in situations with multiple levels of groups,
    -- i.e.:
    --
    -- @
    -- list = group List $ '[' --> exprs --> ']'
    -- tuple = group Tuple $ '(' --> exprs --> ')'
    -- @
    --
    -- with source:
    --
    -- @
    -- [1, (2, ]
    -- @
    --
    -- To handle this scenario, we introduce an unwinding behavior to travel up
    -- through 'group' rules until an appropriate point is found to recover. We
    -- do this first by defining the extended follow set of a group rule @R@ as
    -- the union of the follow sets of @R@ and its ancestor group rules. In the
    -- above example, after the comma the follow set is @[expression]@, but the
    -- extended follow set is @[expression, comma, rparen, rsquare]@.
    --
    -- Unwinding starts when:
    --
    -- (1) The next token __is not__ in the follow set of the current rule
    -- (2) The next token __is__ in the extended follow set of the current rule
    --
    -- While unwinding, the parser will stop at each group ancestor of the
    -- original rule to determine whether to continue unwinding. Unwinding stops
    -- when a group rule @S@ is reached such that the follow set of @S@ includes
    -- the next token. At this point, the same thing happens as in the first
    -- description of error recovery.
    --
    -- The example rules and source would produce the following parse tree and
    -- errors:
    --
    -- @
    -- [List ["1", ",", Tuple ["(", "2", ","], "]"]]
    --
    -- [Unexpected "]" at line 1 column 9: expected expression]
    -- @

    -- * Future improvements

    -- | TODO: First improvement is to move error recovery from the 'group' rule
    -- to the '-->' rule:
    --
    -- > Check if token we error on can be matched by any following grammar rule, if
    -- > so, don't consume that token. Otherwise, do consume it.
    --
    -- While this works well in many cases, it can do what feels like the
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
import Wasp.Backend.ConcreteSyntax (SyntaxKind, SyntaxNode (SyntaxNode, snodeChildren, snodeKind, snodeWidth))
import qualified Wasp.Backend.ConcreteSyntax as S
import Wasp.Backend.ParseError
import Wasp.Backend.Token (Token (tokenKind, tokenWidth), TokenKind, tokenKindIsTrivia)
import qualified Wasp.Backend.Token as T
import Wasp.Backend.TokenSet (TokenSet)
import qualified Wasp.Backend.TokenSet as TokenSet

-- | A monoidal grammar type. Use combinators and primitives to build more
-- complex grammars. Run with `parse`, which supports reasonable automatic error
-- recovery.
data GrammarRule
  = -- | Always succeeds, consuming no input
    Succeed
  | -- | Consumes and produces a node when the next token is the specified kind.
    -- Also consumes and produces any whitespace (even on failure).
    --
    -- On failure, does not consume any input (except whitespace).
    Primitive TokenKind
  | -- | Succeeds at the end of a file
    EOF
  | -- | Run the inner grammar rule and place its resulting node in a node
    -- labelled by the given kind. If it fails, try recovering:
    --
    -- 1. If the failed token is in top `pstateFollowing`, then produce an error
    -- but do not consume the token.
    --
    -- 2. Otherwise, produce an error and consume the token.
    Group SyntaxKind GrammarRule
  | -- | @"As" skind tkind@ is like @"Primitive" tkind@, except the resulting
    -- syntax node is labeled by @skind@ instead of @tkind@.
    As SyntaxKind TokenKind
  | -- | Run the first grammar rule, then the second grammar rule (aborts early
    -- if the first grammar rule fails
    Chain GrammarRule GrammarRule
  | -- | Tries the left grammar rule first. If it fails, backtrack and try the right
    -- grammar rule
    Alternative GrammarRule GrammarRule
  deriving (Eq, Ord, Show)

instance Semigroup GrammarRule where
  x <> y = Alternative x y

instance Monoid GrammarRule where
  mempty = Succeed

-- | Run a "GrammarRule" on an input of tokens. Returns a tuple @(errors, nodes)@.
parse :: [Token] -> GrammarRule -> ([ParseError], [SyntaxNode])
parse tokens grammar =
  let initialState =
        ParseState
          { pstateInput = tokens,
            pstateNodes = [],
            pstateErrors = [],
            pstateNextOffset = 0,
            pstateLastOffset = 0,
            pstateFollowing = []
          }
   in case runState (runExceptT (doParse grammar)) initialState of
        (Left err, state) -> case err of
          Unwind _ -> error "Unwind at top-level GrammarRule (impossible, this is a bug in Wasp.Backend.ConcreteParser.Internal)"
          ParseError perr ->
            let errs = pstateErrors state ++ [perr]
                newNode = errorNode perr False
                nodes = pstateNodes state ++ [newNode]
             in (errs, nodes)
        (_, state) -> (pstateErrors state, pstateNodes state)

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
    -- first grammar rule in a "Chain".
    --
    -- A member @None@ represents EOF.
    pstateFollowing :: [TokenSet]
  }
  deriving (Eq, Show, Ord)

data ParseException = Unwind (Maybe TokenKind) | ParseError ParseError

-- | Parsing monad
type ParserM a = ExceptT ParseException (State ParseState) a

-- | Run a "GrammarRule" in the "ParserM" monad.
doParse :: GrammarRule -> ParserM ()
doParse Succeed = parseSucceed
doParse (Primitive kind) = parsePrimitive kind
doParse EOF = parseEOF
doParse (As label kind) = parseAs label kind
doParse (Group label inner) = parseGroup label inner
doParse (Chain first second) = parseChain first second
doParse (Alternative left right) = parseAlternative left right

-- | Parse "Succeed" rule by consuming whitespace and then succeeding
parseSucceed :: ParserM ()
parseSucceed = consumeWhitespace

-- | Parse "Primitive" rule. Checks whether the first non-trivia token in the
-- remaining input is the correct kind. Outputs a syntax node if it is, otherwise
-- reports an error, without consuming the next non-trivia token.
parsePrimitive :: TokenKind -> ParserM ()
parsePrimitive kind =
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
    nextToken -> makeError nextToken (TokenSet.fromKind kind)

-- | Parse "EOF" rule. If there are no non-trivia tokens in the remaining input,
-- this parser succeeds without making any nodes. Otherwise, it consumes all
-- remaining input and reports an error at the first non-trivia token.
parseEOF :: ParserM ()
parseEOF =
  (consumeWhitespace >> peek) >>= \case
    Just nextToken -> do
      startOffset <- gets pstateNextOffset
      advance
      let firstTokenKind = tokenKind nextToken
      -- TODO: consider reporting all kinds here? not sure
      _tokenKinds <- collectUntilEOF
      throwError $
        ParseError $
          Unexpected
            (Span startOffset (startOffset + tokenWidth nextToken))
            firstTokenKind
            TokenSet.fromEOF
      where
        collectUntilEOF :: ParserM [TokenKind]
        collectUntilEOF =
          peek >>= \case
            Just next -> advance >> ((tokenKind next :) <$> collectUntilEOF)
            Nothing -> pure []
    Nothing -> do
      return ()

-- | Parse "As" rule. Identical to "Primitive", but uses the label as the kind
-- for the syntax node when it succeeds.
parseAs :: SyntaxKind -> TokenKind -> ParserM ()
parseAs label kind =
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
    nextToken -> makeError nextToken (TokenSet.fromKind kind)

-- | Parse "Group" rule. Runs the inner parser. When it succeeds, it puts the
-- outputted nodes from that parser under a parent node with the label. When it
-- fails, it recovers from the error by catching the error or unwinding.
parseGroup :: SyntaxKind -> GrammarRule -> ParserM ()
parseGroup label inner = do
  let pushGroupNode children =
        let width = sum $ map snodeWidth children
         in pushNode $
              SyntaxNode
                { snodeKind = label,
                  snodeWidth = width,
                  snodeChildren = children
                }

  consumeWhitespace
  siblingNodes <- gets pstateNodes
  modify (\s -> s {pstateNodes = []})
  result <- tryError $ doParse inner
  childNodes <- gets pstateNodes
  modify (\s -> s {pstateNodes = siblingNodes})
  case result of
    Nothing -> do
      pushGroupNode childNodes
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
                (following : _) -> kind `TokenSet.member` following

          pushGroupNode childNodes

          if errKindIsImmediatelyFollowing
            then return ()
            else throwError (Unwind kind) -- Re-throw unwind
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
          let errKindIsFollowingKind = any (errKind `TokenSet.member`) followingStack
          let errKindIsImmediatelyFollowing = case followingStack of
                [] -> False -- technically unreachable, but this is sensible (TODO: consider using error here?)
                (following : _) -> errKind `TokenSet.member` following

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
            throwError $ Unwind errKind

-- | Parse "Chain" rule. Runs the first and then the second parser.
parseChain :: GrammarRule -> GrammarRule -> ParserM ()
parseChain first second = do
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

-- | Parse "Alternative" rule. If the left parser can consume the next token in
-- the input, run that parser. If the right parser can consume the next token,
-- run that parser. Otherwise, report an error.
parseAlternative :: GrammarRule -> GrammarRule -> ParserM ()
parseAlternative left right = do
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
        else makeError nextToken (leftFirstTokens `TokenSet.union` rightFirstTokens)

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
    tok : _ -> return (Just tok)

-- | Advance past the next token in the input, updating offsets.
advance :: ParserM ()
advance =
  gets pstateInput >>= \case
    [] -> modify (\s -> s {pstateLastOffset = pstateNextOffset s})
    (consumed : remaining) -> do
      newLastOffset <- gets pstateNextOffset
      let newNextOffset = newLastOffset + tokenWidth consumed
      modify (\s -> s {pstateInput = remaining, pstateLastOffset = newLastOffset, pstateNextOffset = newNextOffset})

-- | @makeError actual expected@ throws an error.
--
-- Assumes @actual@ has not been `advance`d past yet (and does not advance past
-- it).
makeError :: Maybe Token -> TokenSet -> ParserM ()
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
pushFollowing :: TokenSet -> ParserM ()
pushFollowing eset = modify (\s -> s {pstateFollowing = eset : pstateFollowing s})

-- | Remove the last added "ExpectedSet" from the following stack.
popFollowing :: ParserM ()
popFollowing = modify (\s -> s {pstateFollowing = drop 1 $ pstateFollowing s})

-- | Check if a "TokenKind" (or end of file) is one of the first possible tokens
-- in a "GrammarRule".
willSucceedIn :: Maybe TokenKind -> GrammarRule -> Bool
_ `willSucceedIn` Succeed = True
k `willSucceedIn` grammarRule = k `TokenSet.member` findFirstTokens grammarRule

-- | Find the "ExpectedSet" of "TokenKind"s that the "GrammarRule" would succeed on
-- as its first input.
findFirstTokens :: GrammarRule -> TokenSet
findFirstTokens grammarRule = fst $ go grammarRule TokenSet.empty
  where
    -- Returns (firstTokens, can succeed with no input?)
    go :: GrammarRule -> TokenSet -> (TokenSet, Bool)
    go Succeed eset = (eset, True)
    go (Primitive kind) eset = (kind `TokenSet.insertKind` eset, False)
    go EOF eset = (TokenSet.insertEof eset, False)
    go (Group _ p) eset = go p eset
    go (As _ kind) eset = (kind `TokenSet.insertKind` eset, False)
    go (Chain first second) eset =
      let (eset', firstCanBeEmpty) = go first eset
       in if firstCanBeEmpty
            then go second eset'
            else (eset', False)
    go (Alternative left right) eset =
      let (eset', leftCanBeEmpty) = go left eset
          (eset'', rightCanBeEmpty) = go right eset'
       in (eset'', leftCanBeEmpty || rightCanBeEmpty)

-- Combinators

infixr 9 -->

-- | Run grammar rules one after another.
--
-- __Example:__
--
-- >>> kwTrue --> kwFalse
-- -- Results in a grammar rule that will match inputs like "true false"
(-->) :: GrammarRule -> GrammarRule -> GrammarRule
first --> second = Chain first second

infix 9 `as`

-- | Matches a single token of the specified "TokenKind" and produce an output
-- node with the given "SyntaxKind".
--
-- __Example:__
--
-- >>> parse [T.Token T.Identifier 3] $ T.Identifier `as` S.DeclName
-- (...,[],[SyntaxNode {snodeKind = DeclName, snodeWidth = 3, snodeChildren = []}])
as :: TokenKind -> SyntaxKind -> GrammarRule
tkind `as` skind = As skind tkind

-- | Runs a grammar rule and places the nodes that grammar rule adds to the
-- output in a parent node.
--
-- __Example:__
--
-- >>> parse [T.Token T.Identifier 3, T.Token T.Int 2] $ group S.Decl (identifier --> int)
-- (...,[],[SyntaxNode {snodeKind = Decl, snodeWidth = 5, snodeChildren = [SyntaxNode {snodeKind = Token Identifier, snodeWidth = 3, snodeChildren = []},SyntaxNode {snodeKind = Token Int, snodeWidth = 2, snodeChildren = []}]}])
group :: SyntaxKind -> GrammarRule -> GrammarRule
group kind grammarRule = Group kind grammarRule

-- | Run a grammar rule or consume no input.
--
-- __Example:__
--
-- >>> parse [T.Token T.Int 1, T.Token T.Int 1] $ int --> perhaps comma --> int
-- (...,[],[SyntaxNode {snodeKind = Token Int, snodeWidth = 1, snodeChildren = []},SyntaxNode {snodeKind = Token Int, snodeWidth = 1, snodeChildren = []}])
perhaps :: GrammarRule -> GrammarRule
perhaps grammarRule = grammarRule <> Succeed

-- Token grammar rules

lparen :: GrammarRule
lparen = Primitive T.LParen

rparen :: GrammarRule
rparen = Primitive T.RParen

lsquare :: GrammarRule
lsquare = Primitive T.LSquare

rsquare :: GrammarRule
rsquare = Primitive T.RSquare

lcurly :: GrammarRule
lcurly = Primitive T.LCurly

rcurly :: GrammarRule
rcurly = Primitive T.RCurly

comma :: GrammarRule
comma = Primitive T.Comma

colon :: GrammarRule
colon = Primitive T.Colon

kwImport :: GrammarRule
kwImport = Primitive T.KwImport

kwFrom :: GrammarRule
kwFrom = Primitive T.KwFrom

-- | Produces a "S.BoolTrue" syntax node
kwTrue :: GrammarRule
kwTrue = T.KwTrue `as` S.BoolTrue

-- | Produces a "S.BoolFalse" syntax node
kwFalse :: GrammarRule
kwFalse = T.KwFalse `as` S.BoolFalse

-- | Produces a "S.String" syntax node
string :: GrammarRule
string = T.String `as` S.String

-- | Produces a "S.Int" syntax node
int :: GrammarRule
int = T.Int `as` S.Int

-- | Produces a "S.Double" syntax node
double :: GrammarRule
double = T.Double `as` S.Double

lquote :: GrammarRule
lquote = Primitive T.LQuote

rquote :: GrammarRule
rquote = Primitive T.RQuote

quoted :: GrammarRule
quoted = Primitive T.Quoted

-- | Succeeds when the parser is at the end of the file.
eof :: GrammarRule
eof = EOF

-- UTILITIES:

tryError :: MonadError e m => m () -> m (Maybe e)
tryError m = (m >> return Nothing) `catchError` (return . Just)
