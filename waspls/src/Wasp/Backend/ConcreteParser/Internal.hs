{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Wasp.Backend.ConcreteParser.Internal
  ( -- * Internal parsing library
    GrammarRule,
    parse,

    -- * Combinators

    -- | The fixity of the operators ('(-->)', 'as', '(<$$>)', and '(<|>)') was
    -- chosen to make using operators require as little parentheses as possible:
    --
    -- >>> Program <$$> lparen --> rparen
    --       <|> string
    -- Alternative (Group Program (Chain (Token LParen) (Token RParen))) (Token String)
    --
    -- It's still recommended to use parentheses when white space does not make
    -- the meaning extremely clear.
    (-->),
    as,
    (<$$>),
    perhaps,
    (<|>),

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
    -- recovery occurs when an error is produced inside of a group rule. Recovery
    -- is based on the possible tokens that can follow the current rule.
    --
    -- At a basic level, we can describe the mechanism as follows:
    --
    -- For a "GrammarRule" @label '<$$>' first '-->' next@, if an error occurs
    -- while parsing @first@, check if the next token is in the "follow set" of
    -- the group (the follow set of a rule is the set of tokens that can appear
    -- directly after the rule in valid source). If it is, register an error in
    -- the output and leave the token in the input. Otherwise, register an error and
    -- discard the token.
    --
    -- As an example, consider the grammar rules:
    --
    -- @
    -- stmts = eof <|> (stmt --> stmts)
    -- stmt = Stmt <$$> "let" --> identifier --> "=" --> expr --> ";"
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
    -- discarded by the group. This leaves it available for the recursive call to
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
    -- list = List <$$> '[' --> exprs --> ']'
    -- tuple = Tuple <$$> '(' --> exprs --> ')'
    -- @
    --
    -- with source:
    --
    -- @
    -- [1, (2, ]
    -- @
    --
    -- To handle this scenario, we introduce an unwinding behavior to travel up
    -- through group rules until an appropriate point is found to recover. We
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

    -- | TODO: First improvement is to move error recovery from the group rule
    -- to the '-->' rules:
    --
    -- > Check if token we error on can be matched by any following grammar rule, if
    -- > so, leave the token for the following rule. Otherwise, discard the token.
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
    -- current system identifies @page@ as a variable for the dictionary entry
    -- @to:@. Then it treats @TestPage@ as the key for another entry, and
    -- complains that it found @{@ instead of @:@.
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

-- | The type of a grammar rule. Use combinators and primitives to build more
-- complex grammars. Run with `parse`, which supports reasonable automatic error
-- recovery.
--
-- Every rule begins parsing by consuming trivia tokens at the beginning of the
-- input and creating a syntax node for each trivia token.
data GrammarRule
  = -- | Always succeeds, consuming only trivia tokens.
    Succeed
  | -- | Checks if the first non-trivia token is the expected token
    -- kind. If it is, it consumes the token and output a syntax node. Otherwise,
    -- it does not consume the token and throws a parse error exception.
    Token TokenKind
  | -- | Succeeds if the input is empty after consuming trivia tokens at the
    -- beginning of the input.
    --
    -- If it fails, it will report an error at the first non-trivia token while
    -- also consuming all remaining input.
    EOF
  | -- | Run the inner grammar rule and wraps its resulting node in a node
    -- labelled by the given kind. If it fails, try recovering:
    --
    -- 1. If the failed token is in top `pstateFollowing`, then produce an error
    -- but do not consume the token.
    --
    -- 2. Otherwise, produce an error and consume the token.
    --
    -- This is the only rule that adds structure and depth into the CST.
    Group SyntaxKind GrammarRule
  | -- | @"As" skind tkind@ is like @"Token" tkind@, except the resulting
    -- syntax node is labeled by @skind@ instead of @tkind@.
    As SyntaxKind TokenKind
  | -- | Run the first grammar rule. If it succeeds, run the second rule.
    Chain GrammarRule GrammarRule
  | -- | Run the grammar rule that can accept the next token in the input as its
    -- first token. If both rules can be used, it chooses the left rule. Because
    -- of this, avoid making alternatives between rules that can be ambiguous
    -- in this way.
    --
    -- TODO: Try running each rule and use backtracking on failure
    Alternative GrammarRule GrammarRule
  deriving (Eq, Ord, Show)

-- | Run a "GrammarRule" on an input of tokens. Returns a tuple @(errors, nodes)@.
parse :: [Token] -> GrammarRule -> ([ParseError], [SyntaxNode])
parse tokens grammar =
  let initialState =
        ParseState
          { pstateInput = tokens,
            pstateNodes = [],
            pstateErrors = [],
            pstateNextTokenOffset = 0,
            pstatePrevTokenOffset = 0,
            pstateFollowingTokens = []
          }
   in case runState (runExceptT (parseRule grammar)) initialState of
        (Left err, state) -> case err of
          Unwind _ -> error "Unwind at top-level GrammarRule (impossible, this is a bug in Wasp.Backend.ConcreteParser.Internal)"
          ParseError perr ->
            let errs = pstateErrors state ++ [perr]
                newNode = errorNode perr False
                nodes = pstateNodes state ++ [newNode]
             in (errs, nodes)
        (_, state) -> (pstateErrors state, pstateNodes state)

data ParseState = ParseState
  { -- | Remaining tokens in input.
    pstateInput :: [Token],
    -- | "SyntaxNode"s to be used as children for a group node or as the top-level nodes.
    --
    -- When a group rule is entered, the field is replaced with an empty list. After
    -- running the inner rule, this field is used as the group's children and the
    -- original value is restored.
    pstateNodes :: [SyntaxNode],
    -- | Errors collected that will be reported.
    pstateErrors :: [ParseError],
    -- | 0-based offset from the beginnning of the source file, in characters,
    -- of the start of the next token (@head . `pstateInput`@).
    pstateNextTokenOffset :: !Int,
    -- | 0-based offset from the beginnning of the source file, in characters,
    -- of the start of the last consumed token
    pstatePrevTokenOffset :: !Int,
    -- | Stack of possible first tokens, collected when we enter a group as the
    -- first grammar rule in a "Chain".
    --
    -- A member @None@ represents EOF.
    pstateFollowingTokens :: [TokenSet]
  }
  deriving (Eq, Show, Ord)

-- | Two kinds of exceptions are used by the parser:
--
-- (1) 'Unwind': Thrown and caught to implement the error recovery unwinding
-- behavior as described in the module documentation.
--
-- (2) 'ParseError': Thrown when a parse error is encountered. This can be caught
-- and stored in 'pstateErrors' during error recovery.
data ParseException = Unwind (Maybe TokenKind) | ParseError ParseError

type ParserM a = ExceptT ParseException (State ParseState) a

-- | Run a "GrammarRule" in the "ParserM" monad.
parseRule :: GrammarRule -> ParserM ()
parseRule Succeed = parseSucceed
parseRule (Token expectedKind) = parseToken expectedKind
parseRule EOF = parseEOF
parseRule (As label kind) = parseAs label kind
parseRule (Group label inner) = parseGroup label inner
parseRule (Chain first second) = parseChain first second
parseRule (Alternative left right) = parseAlternative left right

parseSucceed :: ParserM ()
parseSucceed = consumeTrivia

parseToken :: TokenKind -> ParserM ()
parseToken expectedKind =
  (consumeTrivia >> peek) >>= \case
    Just nextToken | tokenKind nextToken == expectedKind -> do
      advance
      let newNode =
            SyntaxNode
              { snodeKind = S.Token expectedKind,
                snodeWidth = tokenWidth nextToken,
                snodeChildren = []
              }
      pushNode newNode
    nextToken -> throwParseError nextToken (TokenSet.fromKind expectedKind)

parseEOF :: ParserM ()
parseEOF =
  (consumeTrivia >> peek) >>= \case
    Just nextToken -> do
      nextTokenOffset <- gets pstateNextTokenOffset
      advance
      let nextTokenKind = tokenKind nextToken
      -- TODO: consider reporting all kinds here? not sure
      _tokenKinds <- collectUntilEOF
      throwError $
        ParseError $
          Unexpected
            (Span nextTokenOffset (nextTokenOffset + tokenWidth nextToken))
            nextTokenKind
            TokenSet.fromEOF
      where
        collectUntilEOF :: ParserM [TokenKind]
        collectUntilEOF =
          peek >>= \case
            Just next -> advance >> ((tokenKind next :) <$> collectUntilEOF)
            Nothing -> pure []
    Nothing -> do
      return ()

parseAs :: SyntaxKind -> TokenKind -> ParserM ()
parseAs label kind =
  (consumeTrivia >> peek) >>= \case
    Just nextToken | tokenKind nextToken == kind -> do
      advance
      let newNode =
            SyntaxNode
              { snodeKind = label,
                snodeWidth = tokenWidth nextToken,
                snodeChildren = []
              }
      pushNode newNode
    nextToken -> throwParseError nextToken (TokenSet.fromKind kind)

parseGroup :: SyntaxKind -> GrammarRule -> ParserM ()
parseGroup label inner = do
  consumeTrivia
  previousNodes <- gets pstateNodes
  modify (\s -> s {pstateNodes = []})
  maybeInnerParserError <- tryError $ parseRule inner
  childNodes <- gets pstateNodes
  modify (\s -> s {pstateNodes = previousNodes})
  case maybeInnerParserError of
    Nothing -> pushGroupNode childNodes
    Just (ParseError parseError) -> handleInnerParseError childNodes parseError
    Just (Unwind nextTokenKind) -> handleInnerUnwind childNodes nextTokenKind
  where
    handleInnerParseError :: [SyntaxNode] -> ParseError -> ParserM ()
    handleInnerParseError childNodes parseError = do
      -- ASSUMPTION: The token errored on has not been consumed
      --
      -- (1) Check whether the kind we errored on is in the first `pstateFollowingTokens`
      -- list.
      --
      -- (2) If if it is not in the following stack, consume the error token.
      --
      -- (3) Report the error and create the group node
      --
      -- (4) If the kind is not in first "TokenSet" in the following stack, start
      -- unwinding.

      -- (1)
      followingTokenStack <- gets pstateFollowingTokens
      let expectedKind = case parseError of
            UnexpectedEOF _ _ -> Nothing
            Unexpected _ k _ -> Just k
      let expectedKindIsFollowingKind = any (expectedKind `TokenSet.member`) followingTokenStack
      let expectedKindIsImmediatelyFollowing = case followingTokenStack of
            [] -> False -- technically unreachable, but this is sensible (TODO: consider using error here?)
            (following : _) -> expectedKind `TokenSet.member` following

      -- (2)
      nodeForErr <-
        if expectedKindIsFollowingKind
          then do
            return $ errorNode parseError True
          else do
            advance
            return $ errorNode parseError False

      -- (3)
      pushError parseError
      pushGroupNode $ childNodes ++ [nodeForErr]

      -- (4)
      when (expectedKindIsFollowingKind && not expectedKindIsImmediatelyFollowing) $ do
        throwError $ Unwind expectedKind

    handleInnerUnwind :: [SyntaxNode] -> Maybe TokenKind -> ParserM ()
    handleInnerUnwind childNodes nextTokenKind = do
      -- When unwinding, check if this is the level where we have the
      -- following token. If it is, then we can get rid of the unwind.
      --
      -- Otherwise, we re-throw the unwind
      followingTokenStack <- gets pstateFollowingTokens
      let nextTokenKindIsImmediatelyFollowing = case followingTokenStack of
            [] -> False -- technically unreachable, but this is sensible (TODO: consider using error here?)
            (following : _) -> nextTokenKind `TokenSet.member` following

      pushGroupNode childNodes

      if nextTokenKindIsImmediatelyFollowing
        then return ()
        else throwError (Unwind nextTokenKind)

    pushGroupNode :: [SyntaxNode] -> ParserM ()
    pushGroupNode children =
      let width = sum $ map snodeWidth children
       in pushNode $
            SyntaxNode
              { snodeKind = label,
                snodeWidth = width,
                snodeChildren = children
              }

parseChain :: GrammarRule -> GrammarRule -> ParserM ()
parseChain first second = do
  consumeTrivia
  -- First, figure out following set for @second@
  let following = getValidFirstTokens second
  pushFollowingTokens following
  maybeError <- tryError (parseRule first)
  -- Make sure to remove following set now that we're leaving this "Chain"
  popFollowingTokens
  -- Run second (or throw error if first failed)
  case maybeError of
    Nothing -> parseRule second
    Just err -> throwError err

parseAlternative :: GrammarRule -> GrammarRule -> ParserM ()
parseAlternative left right = do
  consumeTrivia
  let leftFirstTokens = getValidFirstTokens left
  let rightFirstTokens = getValidFirstTokens right
  nextToken <- peek
  let nextKind = tokenKind <$> nextToken
  if nextKind `willSucceedIn` left
    then parseRule left
    else
      if nextKind `willSucceedIn` right
        then parseRule right
        else throwParseError nextToken (leftFirstTokens `TokenSet.union` rightFirstTokens)

-- | Advance past trivia tokens, making syntax nodes for each token
consumeTrivia :: ParserM ()
consumeTrivia =
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
        consumeTrivia
      | otherwise -> pure ()

-- | Peek the immediate next token in input (including trivia)
peek :: ParserM (Maybe Token)
peek =
  gets pstateInput >>= \case
    [] -> return Nothing
    tok : _ -> return (Just tok)

-- | Advance past the next token in the input, updating offsets.
advance :: ParserM ()
advance =
  gets pstateInput >>= \case
    [] -> modify (\s -> s {pstatePrevTokenOffset = pstateNextTokenOffset s})
    (consumed : remaining) -> do
      newLastOffset <- gets pstateNextTokenOffset
      let newNextOffset = newLastOffset + tokenWidth consumed
      modify (\s -> s {pstateInput = remaining, pstatePrevTokenOffset = newLastOffset, pstateNextTokenOffset = newNextOffset})

-- | @throwParseError actual expected@ throws an error.
--
-- Assumes @actual@ has not been `advance`d past yet (and does not advance past
-- it).
throwParseError :: Maybe Token -> TokenSet -> ParserM ()
throwParseError Nothing eset = do
  offset <- gets pstateNextTokenOffset
  throwError $ ParseError $ UnexpectedEOF offset eset
throwParseError (Just actual) expectedTokens = do
  offset <- gets pstateNextTokenOffset
  throwError $ ParseError $ Unexpected (Span offset (offset + tokenWidth actual)) (tokenKind actual) expectedTokens

-- | Make an error node. Error node should only be produced when catching and
-- pushing errors to the state.
errorNode ::
  -- | Parse error that will be used to calculate the width of the error node.
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

-- | Add an "TokenSet" to the following stack.
pushFollowingTokens :: TokenSet -> ParserM ()
pushFollowingTokens eset = modify (\s -> s {pstateFollowingTokens = eset : pstateFollowingTokens s})

-- | Remove the last added "TokenSet" from the following stack.
popFollowingTokens :: ParserM ()
popFollowingTokens = modify (\s -> s {pstateFollowingTokens = drop 1 $ pstateFollowingTokens s})

-- | Check if a "TokenKind" (or end of file) is one of the first possible tokens
-- in a "GrammarRule".
willSucceedIn :: Maybe TokenKind -> GrammarRule -> Bool
_ `willSucceedIn` Succeed = True
k `willSucceedIn` grammarRule = k `TokenSet.member` getValidFirstTokens grammarRule

-- | Find the "TokenSet" of "TokenKind"s that the "GrammarRule" would succeed on
-- as its first input.
getValidFirstTokens :: GrammarRule -> TokenSet
getValidFirstTokens grammarRule = fst $ go grammarRule TokenSet.empty
  where
    -- Returns (firstTokens, can succeed with no input?)
    go :: GrammarRule -> TokenSet -> (TokenSet, Bool)
    go Succeed eset = (eset, True)
    go (Token kind) eset = (kind `TokenSet.insertKind` eset, False)
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

infixr 8 -->

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
-- ([],[SyntaxNode {snodeKind = DeclName, snodeWidth = 3, snodeChildren = []}])
as :: TokenKind -> SyntaxKind -> GrammarRule
tkind `as` skind = As skind tkind

infixr 7 <$$>

-- | @kind '<$$>' rule@ (pronounced group) runs a grammar rule and places the
-- nodes that grammar rule adds to the
-- output in a parent node.
--
-- __Example:__
--
-- >>> parse [T.Token T.Identifier 3, T.Token T.Int 2] $ S.Decl <$$> identifier --> int
-- ([],[SyntaxNode {snodeKind = Decl, snodeWidth = 5, snodeChildren = [SyntaxNode {snodeKind = Token Identifier, snodeWidth = 3, snodeChildren = []},SyntaxNode {snodeKind = Token Int, snodeWidth = 2, snodeChildren = []}]}])
(<$$>) :: SyntaxKind -> GrammarRule -> GrammarRule
kind <$$> grammarRule = Group kind grammarRule

-- | Run a grammar rule or consume no input.
--
-- __Example:__
--
-- >>> parse [T.Token T.Int 1, T.Token T.Int 1] $ int --> perhaps comma --> int
-- ([],[SyntaxNode {snodeKind = Token Int, snodeWidth = 1, snodeChildren = []},SyntaxNode {snodeKind = Token Int, snodeWidth = 1, snodeChildren = []}])
perhaps :: GrammarRule -> GrammarRule
perhaps grammarRule = grammarRule <|> Succeed

infixr 6 <|>

-- | Run a grammar, choosing either the left or the right rule.
--
-- __Example:__
--
-- >>> let number = int <|> double
-- >>> parse [T.Token T.Int 2] number
-- ([],[SyntaxNode (Token T.Int) 2 []])
-- >>> parse [T.Token  T.Double 3] number
-- ([],[SyntaxNode (Token T.Double) 2 []])
(<|>) :: GrammarRule -> GrammarRule -> GrammarRule
left <|> right = Alternative left right

-- Primitive grammar rules

lparen :: GrammarRule
lparen = Token T.LParen

rparen :: GrammarRule
rparen = Token T.RParen

lsquare :: GrammarRule
lsquare = Token T.LSquare

rsquare :: GrammarRule
rsquare = Token T.RSquare

lcurly :: GrammarRule
lcurly = Token T.LCurly

rcurly :: GrammarRule
rcurly = Token T.RCurly

comma :: GrammarRule
comma = Token T.Comma

colon :: GrammarRule
colon = Token T.Colon

kwImport :: GrammarRule
kwImport = Token T.KwImport

kwFrom :: GrammarRule
kwFrom = Token T.KwFrom

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
lquote = Token T.LQuote

rquote :: GrammarRule
rquote = Token T.RQuote

quoted :: GrammarRule
quoted = Token T.Quoted

-- | Succeeds when the parser is at the end of the file.
eof :: GrammarRule
eof = EOF

-- UTILITIES:

tryError :: MonadError e m => m () -> m (Maybe e)
tryError m = (m >> return Nothing) `catchError` (return . Just)
