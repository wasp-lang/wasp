module Wasp.Analyzer
  ( -- * Overview

    -- |
    -- "Analyzer" is the first half (\"frontend\") of Wasp compiler, with "Generator" being the second half (\"backend\").
    --
    -- Analyzer parses the Wasp source into "Wasp" AST, which is then passed on to the "Generator".
    --
    -- It consists of three stages:
    --
    --  1. "Analyzer.Parser", which transforms source text into "Analyzer.Parser.AST",
    --     which captures basic structure like literals, lists, dictionaries, declarations, ...,
    --     without getting too much into details of them (it doesn't care about types, or exact declaration types, ...).
    --  2. "Analyzer.TypeChecker", which transforms "Analyzer.Parser.AST" into "Analyzer.TypeChecker.AST",
    --     which is almost the same as parser AST but is enriched with type information (what is of which type).
    --     While enriching AST with type information, type checker also checks that actual types match expected types.
    --     Wasp is strongly statically typed language.
    --  3. "Analyzer.Evaluator", which transforms "Analyzer.TypeChecker.AST" into "Wasp" AST.
    --     Wasp AST is output of "Analyzer" and input into "Generator", and is central AST in Wasp compiler.

    -- ** Wasp's type system

    -- |
    -- Wasp's type system can be divided into two main categories of types:
    --
    --  1. __fundamental__ types, which we can further divide into primitive types
    --     (@string@, @bool@, @integer@, ...) and composite types (@dict@ and @list@).
    --  2. __domain__ types (e.g. @page@, @route@, ...), which can be many and various
    --     but are always of either Decl (Declaration) kind or of Enum (Enumeration) kind (kind is a type of a type).
    --
    -- While fundamental types are here to be basic building blocks of a language,
    -- domain types are here to bring the D into the DSL, to model the concepts of a web app like
    -- @page@, @route@, @query@, @entity@, @AuthMethod@, ... .
    --
    -- Primitive fundamental types contain no other types, composite fundamental types can contain any other type,
    -- and domain types may contain some other types (depends on the shape of a specific domain type).
    --
    -- For example, in the following fictional snippet of Wasp code
    --
    -- > app MyApp {
    -- >   title: "My App",
    -- >   homePage: DashboardPage,
    -- >   auth: UsernameAndPassword
    -- > }
    -- >
    -- > page DashboardPage { ... }
    --
    -- we have @MyApp@, a value of domain type @app@.
    -- Domain type @app@ is of kind Declaration and its definition is a dict with fields
    -- @auth@, @homePage@ and @title@ (and possibly some more optional fields).
    -- @title@ field is of fundamental type @string@.
    -- @homePage@ field is of domain type @page@ which is a Declaration.
    -- @auth@ field is of domain type @AuthMethod@ which is an Enum (one of whose values is @UsernameAndPassword@).

    -- *** Domain types and how to define them

    -- |
    -- Domain types are specific in a sense that there are two kinds of them (kind is a type of a type):
    -- Decl (Declaration) and Enum (Enumeration).
    --
    -- "Analyzer" is implemented in such a way that it is aware of this and can work with any number and variety
    -- of domain types that are of Decl or Enum kind, as long as enough information is provided about them.
    -- This means that while fundamental types are hardcoded in "Analyzer",
    -- domain types are much more flexible and we can easily modify, add or remove them in our code
    -- (in Wasp compiler code, which is what you are looking at right now, not actual Wasp language code (.wasp))
    -- without doing almost any changes to the "Analyzer", which is what enables us to quickly iterate on the Wasp language.
    --
    -- This \"dependency injection\" of domain types into "Analyzer" is accomplished through
    -- "Analyzer.TypeDefinitions" module, which defines a 'Analyzer.TypeDefinitions.TypeDefinitions'
    -- type which is a container for 'DeclType' and 'EnumType' which each define one domain type:
    -- 'DeclType' for a domain type of Decl kind, and 'EnumType' for a domain type of Enum kind.
    --
    -- So for example if Wasp language has domain types @page@ and @route@ which are declarations and
    -- @AuthMethod@ which is enum, we will inject "Analyzer" with a value of type 'TypeDefinitions'
    -- that has two 'DeclType' values, one describing @page@ and one describing @route@,
    -- and one 'EnumType' value that describes @AuthMethod@.
    --
    -- 'DeclType' (or 'EnumType') describes a specific type (e.g. @page@) in two distinct ways:
    --
    --  1. What is its type shaped like in Wasp type system (e.g. it could be a dict of smth).
    --  2. How it evaluates from a node in typechecked AST into the node in Wasp AST.
    --
    -- "Wasp" AST is output of "Analyzer" and input into "Generator", and it is central AST in Wasp compiler.
    -- It is not tied to "Analyzer" or "Generator" -> it is standalone and directly describes the web app domain.
    -- Nodes in "Wasp" AST directly correspond to a Wasp domain types,
    -- with top level nodes in "Wasp" AST always being of Decl kind
    -- (therefore making "Wasp" AST basically a list of declarations).
    -- Therefore, following previous example, if our Wasp language has declarations @page@ and @route@,
    -- there will be @App@ and @Route@ types of nodes in "Wasp" AST.
    --
    -- Therefore, when defining 'TypeDefinitions' to inject into "Analyzer",
    -- we will always be defining them based on "Wasp" AST and types of nodes it has.
    -- Since this is a tedious process (defining manually for each domain type what is its shape
    -- and how it evaluates from typechecked AST into Wasp AST),
    -- we implemented Template Haskell functions ("Analyzer.Evaluator.TH") which generate these automatically
    -- based on "Wasp" AST!
    --
    -- These are 'makeDeclType' and 'makeEnumType', which for a given "Wasp" AST node type (e.g. @data Page@)
    -- automatically create its instance of 'Evaluator.TypeDefinitions.IsDeclType' \/ 'Evaluator.TypeDefinitions.IsEnumType'.
    -- Once this instance is registered with "Analyzer" via "Analyzer.StdTypeDefinitions", that is it ->
    -- Analyzer will know how to create 'DeclType'\/'EnumType' based on it, which gives it all info it needs
    -- to work with that domain type!
    -- Keep in mind that you could define these classes manually if you wish,
    -- but normally it is just boilerplate and doing it via TH functions we implemented is just much easier.

    -- * Diagram

    -- |
    -- Check Analyzer/README.md for a diagram of Analyzer!

    -- * API
    analyze,
    E.takeDecls,
    AnalyzeError (..),
  )
where

import Control.Arrow (left)
import Control.Monad ((>=>))
import Wasp.Analyzer.Evaluator (Decl)
import qualified Wasp.Analyzer.Evaluator as E
import qualified Wasp.Analyzer.Parser as P
import Wasp.Analyzer.StdTypeDefinitions (stdTypes)
import qualified Wasp.Analyzer.TypeChecker as T

data AnalyzeError
  = ParseError P.ParseError
  | TypeError T.TypeError
  | EvaluationError E.EvaluationError
  deriving (Show, Eq)

-- | Takes a Wasp source file and produces a list of declarations or a
--   description of an error in the source file.
analyze :: String -> Either AnalyzeError [Decl]
analyze =
  (left ParseError . P.parse)
    >=> (left TypeError . T.typeCheck stdTypes)
    >=> (left EvaluationError . E.evaluate stdTypes)
