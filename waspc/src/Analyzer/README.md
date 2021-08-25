# High level overview of Analyzer

`Analyzer` is first half ("frontend") of Wasp compiler, with `Generator` being the second half.

`Analyzer` parses the Wasp source into Wasp AST, which is then passed on to the Generator.

`Analyzer` consists of three stages:
1. `Parser`, which transforms source text into `Parser.AST`, which captures basic structure like literals, lists, dictionaries, declarations, ..., without getting too much into details of them.
2. `TypeChecker`, which transforms `Parser.AST` into `TypeChecker.AST`, which is almost the same as `Parser.AST` but is enriched with type information (what is of which type).
   While enriching AST with type information, `TypeChecker` also checks that actual types match expected types. Wasp is strictly and statically typed language.
3. `Evaluator`, which transforms `TypeChecker.AST` into Wasp AST (`Wasps` module). Wasp AST is output of Analyzer and input into Generator, and is central AST in Wasp compiler.

## Wasp's type system

Wasp's type system has "fundamental" types, which we can divide into primitive types (String, Bool, Integer, ...) and composite types (Dict and List), and it also has "domain" types (e.g. `page`, `route`, ...), which can be many and various but are always of either Declaration kind or of Enum kind (kind is a type of type).

While fundamental types are here to be basic building blocks of a language, domain types are here to bring `D` into the `DSL`, to model the concepts of a web app like `page`, `route`, `database`, `entity`, `AuthMethod`, ... .

Primitive fundamental types contain no other types, composite fundamental types can contain any other type, and domain types may contain some other types (depends on a specific domain type).

### Domain types and how to define them

Domain types are specific in a sense that there are two kinds of them (kind is a type of a type), Decl (Declaration) and Enum.
`Analyzer` is implemented in such way that it is aware of this and can work with any number and variety of domain types that are of Decl or Enum kind, as long as enough information is provided about them. This means that while fundamental types are hardcoded in Analyzer, domain types are much more flexible and we can easily modify, add or remove them in our code without doing almost any changes to the Analyzer, which is what enables us to quickly iterate on the language.

This "dependency injection" of domain types into `Analyzer` is accomplished through `Analyzer.TypeDefinitions` module, which defines a `data TypeDefinitions` which is a container for `data DeclType` and `data EnumType` which each define one domain type: `DeclType` for a type of Decl kind, and `EnumType` for a type of Enum kind. So for example if Wasp language has domain types `page` and `route` which are declarations and `AuthMethod` which is enum, we will inject `Analyzer` with a value of type `TypeDefinitions` that has two `DeclType` values, one describing `page` and one describing `route`, and one `EnumType` value that describes `AuthMethod`.

`DeclType`(or `EnumType`) describes a specific type (e.g. `page`) in two distinct ways:
 1. What is it's type shaped like in Wasp type system (usually it is a Dict of smth).
 2. How it evaluates from a node in typechecked AST into the node in Wasp AST.
 
 Wasp AST is AST that is output of Analyzer and input into Generator, and it is central AST in Wasp compiler. It is not tied to Analyzer or Generator -> it is standalone and directly describes the web app domain. Every type of node in Wasp AST directly corresponds to a single fundamental Wasp type, with top level nodes in Wasp AST always being of Declaration kind (therefore making it a list of declarations). Therefore, following previous example, if our Wasp language has declarations `page` and `route`, there will be `App` and `Route` types of nodes in Wasp AST.
 
 Therefore, when defining `TypeDefinitions` to inject into Analyzer, we will always be defining them based on Wasp AST and types of nodes it has.
 Since this is a tedious process (defining manually for each fundamental type what is it's shape and how it evaluates from typechecked AST into Wasp AST), we implemented a Template Haskell functions which generate these automatically based on Wasp AST!
 These are `makeDeclType`/`makeEnumType`, which for a given Wasp AST node type (`e.g. data Page`) automatically create its instance of `Evaluator.TypeDefinitions.IsDeclType`/`Evaluator.TypeDefinitions.IsEnumType`. Once this instance is registered with Analyzer, that is it -> Analyzer will know how to create `DeclType`/`EnumType` based on it, which gives it all info it needs to work with that fundamental type!
Keep in mind that you could define these classes manually if you wish, but normally it is just boilerplate and doing it via TH functions we implemented is just much easier.
