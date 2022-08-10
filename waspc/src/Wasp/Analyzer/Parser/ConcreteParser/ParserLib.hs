{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Wasp.Analyzer.Parser.ConcreteParser.ParserLib
  ( -- * Internal parsing library
    GrammarRule,
    parse,

    -- * Combinators

    -- | The fixity of the operators ('(<>)', '`as`', '(<$$>)', and '(<|>)') was
    -- chosen to make using operators require as little parentheses as possible:
    --
    -- >>> Program <$$> lparen <> rparen
    --       <|> string
    -- Alternative (Group Program (Chain (Token LParen) (Token RParen))) (Token String)
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
    -- First, we define the follow set of a grammar rule @R@ in the grammar
    -- @R <> S@ to be the set of token kinds that @S@ can accept as its first
    -- token. For example, @"let"@ in @"let" <> ("=" <|> "+=")@ has the follow
    -- set @["=", "+="]@.
    --
    -- We can then describe the basic mechanism as follows:
    --
    -- For a "GrammarRule" @label '<$$>' first '<>' next@, if an error occurs
    -- while parsing @first@, check if the token it errored on is in the follow
    -- set for the group.
    --
    -- If yes, then there is a following rule that can accept this token. So
    -- just continue parsing. Otherwise, no following rule has a use for the
    -- token, so discard it.
    --
    -- In either case, we log an error to report at the end of parsing.
    --
    -- As an example, consider the grammar rules:
    --
    -- @
    -- stmts = eof <|> (stmt <> stmts)
    -- stmt = Stmt <$$> "let" <> identifier <> "=" <> expr <> ";"
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
    -- instead of the expected @;@.
    --
    -- First, we find the follow set of the @Stmt@ group. From the definition of
    -- @stmts@, we can see @stmt@ is followed by @stmts@. @stmts@ can start with
    -- either @eof@ or @stmt@ (which in turn can start with @"let"@. Hence, the
    -- follow set is @[eof, "let"]@.
    --
    -- Since the "let" token the parser erroed on is in this set, it does not
    -- get discarded by the group. This leaves it available for the call to
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
    -- But if we look at a more complex situation, with nested groups, we need
    -- to introduce another concept to produce correct parses. For example:
    --
    -- @
    -- list = List <$$> '[' <> expr <> ']'
    -- tuple = Tuple <$$> '(' <> expr <> ')'
    -- expr = list <|> tuple <|> number
    -- @
    --
    -- with source:
    --
    -- @
    -- [(2 ]
    -- @
    --
    -- If we were to apply the same logic as before, we would find the follow
    -- set of the @2@ to be @[")"]@. Since @]@ is not in this set, we discard
    -- the right square bracket. But what we want to do is leave it so that
    -- the @list@ rule can use it as its closing bracket.
    --
    -- To handle this scenario, we introduce an unwinding behavior to travel up
    -- through group rules until an appropriate point is found to recover. We
    -- do this first by defining the extended follow set of a rule @R@ as
    -- the union of the follow sets of @R@ and its ancestor rules.
    --
    -- For example, we find the follow set and extended follow set of the @2@ in
    -- the example code. To start, we write out the tree for grammar rules that
    -- are used:
    --
    -- @
    -- List <$$> '[' <> (Tuple <$$> '(' <> number <> ')') <> ']'
    -- @
    --
    -- Now, we see the follow set of the @2@ is just @[')']@. Then, the follow
    -- set of its ancestors (@Tuple<$$>@ and @List<$$>@) are @[']']@ and @[]@,
    -- respectively. So, the extended follow set is @[')', ']']@.
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
    -- [List ["[", Tuple ["(", "2"], "]"]]
    --
    -- [Unexpected "]" at line 1 column 9: expected expression]
    -- @
    --
    -- ** Error Recovery Rules
    --
    -- A description of the full error recovery process:
    --
    -- 1. While parsing @first@ in @label '<$$>' first '<>' next@, an
    --    unexpected token is encountered.
    -- 2. The follow set and extended follow set for the group node are
    --    calculated.
    -- 3. If the unexpected token is in the follow set, continue parsing normally.
    -- 4. If the unexpected token is not in the follow set, but is in the extended
    --    follow set, begin unwinding:
    --    a. Stop at each group rule ancestor @S@ and compute the follow set of
    --       @S@.
    --    b. If the follow set of @S@ contains the unexpected token, stop
    --       unwinding and resume parsing.
    --    c. Otherwise, keep unwinding.
    -- 5. If the unexpected token is not in the extended follow set, discard the
    --    token and continue parsing.

    -- * Future improvements

    -- |
    -- __TODO #1:__
    --
    -- Move error recovery from the group rule to the '<>' rules.
    --
    -- __TODO #2:__
    --
    -- The current error recovery and the upgraded version described in TODO #1
    -- can do what feels like the wrong thing in some cases. For example,
    -- consider the following invalid wasp code:
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
    -- In case this has performance issues with having to explore lots of possible
    -- parse trees, we could limit how many branches we check, similar to optimizing
    -- DFS for state tree searches (like a chess solver).
    --
    -- __TODO #3:__
    --
    -- Change wasp language a bit to make the start of a declaration unambiguous.
    -- This would make it much easier for the parser to recover from an unfinished
    -- declaration.
    --
    -- Ideas for this:
    --
    -- (1) Make all type names reserved keywords in the grammar
    -- (2) Distinguish between type names and variable names based on the case
    --     of the first letter (lowercase = type, uppercase = variable)
    --
    -- __TODO #4:__
    --
    -- When the next token is not in the follow set of the rule, try discarding
    -- multiple tokens, stopping when we do find a token in the follow set.
  )
where

import Control.Monad (unless)
import Control.Monad.Except (ExceptT, MonadError (catchError, throwError), runExceptT)
import Control.Monad.State.Strict (State, gets, modify, runState)
import Data.Maybe (fromMaybe)
import Wasp.Analyzer.Parser.ConcreteParser.CST (SyntaxKind, SyntaxNode (SyntaxNode, snodeChildren, snodeKind, snodeWidth))
import qualified Wasp.Analyzer.Parser.ConcreteParser.CST as S
import Wasp.Analyzer.Parser.ConcreteParser.ParseError
import Wasp.Analyzer.Parser.Token (Token (tokenKind, tokenWidth), TokenKind, tokenKindIsTrivia)
import qualified Wasp.Analyzer.Parser.Token as T
import Wasp.Analyzer.Parser.TokenSet (TokenSet)
import qualified Wasp.Analyzer.Parser.TokenSet as TokenSet

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
    -- kind. If it is, it consumes the token and outputs a syntax node. Otherwise,
    -- it does not consume the token and throws a parse error exception.
    --
    -- If a "SyntaxKind" is specified, use it to label the syntax node created.
    Token TokenKind (Maybe SyntaxKind)
  | -- | Succeeds if the input is empty after consuming trivia tokens at the
    -- beginning of the input.
    --
    -- If it fails, it will report an error at the first non-trivia token while
    -- also consuming all remaining input.
    EOF
  | -- | Run the inner grammar rule and wraps its resulting nodes in a node
    -- labeled by the given syntax kind. If it fails, apply the error recovery
    -- strategy (this is currently the only place where error recovery is
    -- performed).
    --
    -- This is the only rule that adds structure and depth into the CST.
    Group SyntaxKind GrammarRule
  | -- | Run the first grammar rule. If it succeeds, run the second rule.
    Chain GrammarRule GrammarRule
  | -- | Run the grammar rule that can accept the next token in the input as its
    -- first token.
    --
    -- Invariant: There can be no overlap in which token kinds each parser can
    -- accept. The parser will use a Haskell 'error' in this case.
    --
    -- TODO: Remove the invariant and use backtracking to try each rule
    Alternative GrammarRule GrammarRule
  deriving (Eq, Ord, Show)

-- | @rule1 '<>' rule2@ is a parser that runs first @rule1@, then @rule2@.
-- Equivalent to @Chain rule1 rule2@.
instance Semigroup GrammarRule where
  -- "Succeed" is identity of <>. The second pattern does affect the result tree,
  -- but only by changing where trivia tokens are placed in the tree, which is
  -- not an issue.
  Succeed <> rule = rule
  rule <> Succeed = rule
  first <> second = Chain first second

-- | @mempty@ is a parser that matches an empty string, or a string of only
-- trivia tokens.
instance Monoid GrammarRule where
  mempty = Succeed

-- | Run a "GrammarRule" on an input of tokens. Returns a tuple @(errors, nodes)@.
parse :: [Token] -> GrammarRule -> ([ParseError], [SyntaxNode])
parse tokens grammar =
  let initialState =
        ParseState
          { pstateInput = tokens,
            pstateCurrentLevelNodes = [],
            pstateErrors = [],
            pstateNextTokenOffset = 0,
            pstatePrevTokenOffset = 0,
            pstateAcceptableNextTokens = []
          }
   in case runState (runExceptT (parseRule grammar)) initialState of
        (Left err, state) -> case err of
          Unwind -> error "Unwind at top-level GrammarRule (impossible, this is a bug in Wasp.Analyzer.Parser.ConcreteParser.Internal)"
          ParseError perr ->
            let errs = pstateErrors state ++ [perr]
                newNode = makeErrorNode perr False
                nodes = pstateCurrentLevelNodes state ++ [newNode]
             in (errs, nodes)
        (_, state) -> (pstateErrors state, pstateCurrentLevelNodes state)

data ParseState = ParseState
  { -- | Remaining tokens to be parsed.
    pstateInput :: [Token],
    -- | "SyntaxNode"s processed so far at the current level of the CST. An
    -- initial top-level is created when the parser is started and each "Group"
    -- rule creates a new level.
    pstateCurrentLevelNodes :: [SyntaxNode],
    -- | Errors collected that will be reported.
    pstateErrors :: [ParseError],
    -- | 0-based offset from the beginning of the source file, in characters,
    -- of the start of the next token (@head . `pstateInput`@).
    pstateNextTokenOffset :: !Int,
    -- | 0-based offset from the beginning of the source file, in characters,
    -- of the start of the last consumed token
    pstatePrevTokenOffset :: !Int,
    -- | Stack of acceptable next tokens, where entries to the stack are added
    -- when we enter a group rule for parsing.
    --
    -- The first token set in this list is the current follow set and the union
    -- of all of the token sets is the extended follow set.
    pstateAcceptableNextTokens :: [TokenSet]
  }
  deriving (Eq, Show, Ord)

data ParseException
  = -- | @ParseError parseError@ is used as a signal when an unexpected token is
    -- encountered. This kind of exception is caught by error recovery points
    -- and added to the output list of errors.
    --
    -- This may be converted into "Unwind" if error recovery determines unwinding
    -- should occur.
    ParseError ParseError
  | -- | @Unwind tokenKind@ is used as a signal during error recovery. It is used
    -- to implement the unwinding behavior described at the top of the module.
    --
    -- Essentially, it means "I don't know how to parse this, but I know someone
    -- above me can. Can you? If not, pass it along."
    Unwind

type ParserM a = ExceptT ParseException (State ParseState) a

parseRule :: GrammarRule -> ParserM ()
parseRule Succeed = parseSucceed
parseRule (Token expectedKind maybeLabel) = parseToken expectedKind maybeLabel
parseRule EOF = parseEOF
parseRule (Group label inner) = parseGroup label inner
parseRule (Chain first second) = parseChain first second
parseRule (Alternative left right) = parseAlternative left right

parseSucceed :: ParserM ()
parseSucceed = consumeTrivia

parseToken :: TokenKind -> Maybe SyntaxKind -> ParserM ()
parseToken expectedKind maybeSyntaxKind =
  (consumeTrivia >> peek) >>= \case
    Just nextToken | tokenKind nextToken == expectedKind -> do
      advance
      let newNodeKind = fromMaybe (S.Token expectedKind) maybeSyntaxKind
      let newNode =
            SyntaxNode
              { snodeKind = newNodeKind,
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
      -- TODO: consider reporting all token kinds here? not sure
      _tokenKinds <- collectUntilEOF
      throwError $
        ParseError $
          UnexpectedToken
            (Region nextTokenOffset (nextTokenOffset + tokenWidth nextToken))
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

parseGroup :: SyntaxKind -> GrammarRule -> ParserM ()
parseGroup label inner = do
  consumeTrivia
  currentLevelNodes <- gets pstateCurrentLevelNodes
  modify (\s -> s {pstateCurrentLevelNodes = []})
  maybeInnerParserError <- tryError $ parseRule inner
  childNodes <- gets pstateCurrentLevelNodes
  modify (\s -> s {pstateCurrentLevelNodes = currentLevelNodes})
  case maybeInnerParserError of
    Nothing -> pushGroupNode childNodes
    Just (ParseError parseError) -> handleInnerParseError childNodes parseError
    Just Unwind -> handleInnerUnwind childNodes
  where
    handleInnerParseError :: [SyntaxNode] -> ParseError -> ParserM ()
    handleInnerParseError childNodes parseError = do
      -- ASSUMPTION: The token errored on has not been consumed
      --
      -- (1) Check whether the token kind we errored on can be accepted by any
      -- following rule.
      --
      -- (2) Report the error and create the group node containing the error and
      -- what it parsed before the error.
      --
      -- (3) Handle the error:
      --   (3a) If it can not be accepted by a following rule, discard the error
      --        token.
      --   (3b) If unwinding is required to accept the token, begin unwinding.
      --   (3c) Otherwise, the error can be handled without unwinding, so do
      --        nothing and continue parsing.

      -- (1)
      let errorTokenKind = case parseError of
            UnexpectedEOF _ _ -> Nothing
            UnexpectedToken _ k _ -> Just k
      errorCanBeHandled <- canAnyFollowingRuleAcceptTokenKind errorTokenKind
      errorCanBeHandledWithoutUnwind <- canImmediateFollowingRuleAcceptTokenKind errorTokenKind

      -- (2)
      logError parseError
      let nodeForErr = makeErrorNode parseError errorCanBeHandled
      pushGroupNode $ childNodes ++ [nodeForErr]

      -- (3)
      if not errorCanBeHandled
        then -- (3a)
          advance
        else -- (3b)
          unless errorCanBeHandledWithoutUnwind $ throwError Unwind

    handleInnerUnwind :: [SyntaxNode] -> ParserM ()
    handleInnerUnwind childNodes = do
      errorTokenKind <- fmap tokenKind <$> peek
      -- When unwinding, check if this is the level where we can parse the error
      -- token. If it is, then stop unwinding. Otherwise, we continue unwinding.
      errorCanBeHandledWithoutUnwind <- canAnyFollowingRuleAcceptTokenKind errorTokenKind

      pushGroupNode childNodes

      unless errorCanBeHandledWithoutUnwind $ throwError Unwind

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
  pushFollowingTokens $ getValidFirstTokens second
  maybeError <- tryError (parseRule first)
  -- Make sure to remove following set now that we're leaving this "Chain"
  popFollowingTokens
  -- Run second (or throw error if first failed)
  case maybeError of
    Nothing -> parseRule second
    Just err -> throwError err

parseAlternative :: GrammarRule -> GrammarRule -> ParserM ()
parseAlternative left right
  | leftFirstTokens `TokenSet.intersection` rightFirstTokens /= TokenSet.empty =
    error $
      unlines
        [ "[WARNING] Alternative grammar rule has two rules that can accept the same first token."
            ++ "This will result in the second rule never running on these tokens.",
          "\tLeft Tokens:",
          "\t" ++ show leftFirstTokens,
          "\n\tRight Tokens:",
          "\t" ++ show rightFirstTokens
        ]
  | otherwise = do
    consumeTrivia
    nextToken <- peek
    let nextKind = tokenKind <$> nextToken
    if nextKind `willSucceedIn` left
      then parseRule left
      else
        if nextKind `willSucceedIn` right
          then parseRule right
          else throwParseError nextToken (leftFirstTokens `TokenSet.union` rightFirstTokens)
  where
    leftFirstTokens = getValidFirstTokens left
    rightFirstTokens = getValidFirstTokens right

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
  throwError $ ParseError $ UnexpectedToken (Region offset (offset + tokenWidth actual)) (tokenKind actual) expectedTokens

-- | Make an error node. Error node should only be produced when catching and
-- pushing errors to the state.
makeErrorNode ::
  -- | Parse error that will be used to calculate the width of the error node.
  ParseError ->
  -- | If "True", give the error node a width of 0. Otherwise, use the
  -- width of the error item.
  Bool ->
  SyntaxNode
makeErrorNode (UnexpectedEOF _ _) _ =
  SyntaxNode
    { snodeKind = S.Error,
      snodeWidth = 0,
      snodeChildren = []
    }
makeErrorNode (UnexpectedToken (Region so eo) _ _) zeroWidth =
  SyntaxNode
    { snodeKind = S.Error,
      snodeWidth = if zeroWidth then 0 else eo - so,
      snodeChildren = []
    }

-- | Add a node to the output.
pushNode :: SyntaxNode -> ParserM ()
pushNode node = modify (\s -> s {pstateCurrentLevelNodes = pstateCurrentLevelNodes s ++ [node]})

-- | Add an error to the output (does not throw the error).
logError :: ParseError -> ParserM ()
logError err = modify (\s -> s {pstateErrors = pstateErrors s ++ [err]})

-- | Add an "TokenSet" to the following stack.
pushFollowingTokens :: TokenSet -> ParserM ()
pushFollowingTokens eset = modify (\s -> s {pstateAcceptableNextTokens = eset : pstateAcceptableNextTokens s})

-- | Remove the last added "TokenSet" from the following stack.
popFollowingTokens :: ParserM ()
popFollowingTokens = modify (\s -> s {pstateAcceptableNextTokens = drop 1 $ pstateAcceptableNextTokens s})

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
    go (Token tkind _) eset = (tkind `TokenSet.insertKind` eset, False)
    go EOF eset = (TokenSet.insertEof eset, False)
    go (Group _ p) eset = go p eset
    go (Chain first second) eset =
      let (eset', firstCanBeEmpty) = go first eset
       in if firstCanBeEmpty
            then go second eset'
            else (eset', False)
    go (Alternative left right) eset =
      let (eset', leftCanBeEmpty) = go left eset
          (eset'', rightCanBeEmpty) = go right eset'
       in (eset'', leftCanBeEmpty || rightCanBeEmpty)

-- | Check if any following rule can accept the given "TokenKind". If this is
-- true, then unwinding at the current location will succeed in recovering from
-- an error.
canAnyFollowingRuleAcceptTokenKind :: Maybe TokenKind -> ParserM Bool
canAnyFollowingRuleAcceptTokenKind nextTokenKind = do
  followingTokens <- gets pstateAcceptableNextTokens
  return $ any (TokenSet.member nextTokenKind) followingTokens

-- | Check if the next rule can accept the given "TokenKind". If this is true,
-- then an error can be recovered from at the current context with no unwinding.
--
-- Pre-condition: @pstateAcceptableNextTokens@ is not empty.
canImmediateFollowingRuleAcceptTokenKind :: Maybe TokenKind -> ParserM Bool
canImmediateFollowingRuleAcceptTokenKind nextTokenKind = do
  followingTokens <- gets pstateAcceptableNextTokens
  return $ case followingTokens of
    [] -> error "canImmediateFollowingRuleAcceptTokenKind called with empty pstateAcceptableNextTokens. This is a parser library error."
    (nextTokens : _) -> nextTokenKind `TokenSet.member` nextTokens

infix 9 `as`

-- | Matches a single token of the specified "TokenKind" and produce an output
-- node with the given "SyntaxKind".
--
-- __Example:__
--
-- >>> parse [T.Token T.Identifier 3] $ T.Identifier `as` S.DeclName
-- ([],[SyntaxNode {snodeKind = DeclName, snodeWidth = 3, snodeChildren = []}])
as :: TokenKind -> SyntaxKind -> GrammarRule
tkind `as` skind = Token tkind (Just skind)

infixr 5 <$$>

-- | @syntaxKind '<$$>' rule@ (pronounced group) runs a grammar rule and places the
-- nodes that grammar rule adds to the
-- output in a parent node.
--
-- __Example:__
--
-- >>> parse [T.Token T.Identifier 3, T.Token T.Int 2] $ S.Decl <$$> identifier <> int
-- ([],[SyntaxNode {snodeKind = Decl, snodeWidth = 5, snodeChildren = [SyntaxNode {snodeKind = Token Identifier, snodeWidth = 3, snodeChildren = []},SyntaxNode {snodeKind = Token Int, snodeWidth = 2, snodeChildren = []}]}])
(<$$>) :: SyntaxKind -> GrammarRule -> GrammarRule
label <$$> grammarRule = Group label grammarRule

-- | Run a grammar rule or consume no input.
--
-- __Example:__
--
-- >>> parse [T.Token T.Int 1, T.Token T.Int 1] $ int <> perhaps comma <> int
-- ([],[SyntaxNode {snodeKind = Token Int, snodeWidth = 1, snodeChildren = []},SyntaxNode {snodeKind = Token Int, snodeWidth = 1, snodeChildren = []}])
perhaps :: GrammarRule -> GrammarRule
perhaps grammarRule = grammarRule <|> Succeed

infixr 4 <|>

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
lparen = Token T.LParen Nothing

rparen :: GrammarRule
rparen = Token T.RParen Nothing

lsquare :: GrammarRule
lsquare = Token T.LSquare Nothing

rsquare :: GrammarRule
rsquare = Token T.RSquare Nothing

lcurly :: GrammarRule
lcurly = Token T.LCurly Nothing

rcurly :: GrammarRule
rcurly = Token T.RCurly Nothing

comma :: GrammarRule
comma = Token T.Comma Nothing

colon :: GrammarRule
colon = Token T.Colon Nothing

kwImport :: GrammarRule
kwImport = Token T.KwImport Nothing

kwFrom :: GrammarRule
kwFrom = Token T.KwFrom Nothing

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
lquote = Token T.LQuote Nothing

rquote :: GrammarRule
rquote = Token T.RQuote Nothing

quoted :: GrammarRule
quoted = Token T.Quoted Nothing

-- | Succeeds when the parser is at the end of the file.
eof :: GrammarRule
eof = EOF

-- UTILITIES:

tryError :: MonadError e m => m () -> m (Maybe e)
tryError m = (m >> return Nothing) `catchError` (return . Just)
