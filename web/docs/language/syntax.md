---
title: Syntax
---

Wasp is a declarative, statically typed, domain specific language (DSL).

## Declarations

The central point of Wasp language are **declarations**, and Wasp source is at the end just a bunch of declarations, each of them describing a part of your web app.

```wasp
app MyApp {
  title: "My app"
}

route RootRoute { path: "/", to: DashboardPage }

page DashboardPage {
  component: import Dashboard from "@client/Dashboard.js"
}
```

In the example above we described a web app via three declarations: `app MyApp { ... }`, `route RootRoute { ... }` and `page DashboardPage { ... }`.

Syntax for writing a declaration is `<declaration_type> <declaration_name> <declaration_body>`, where:
- `<declaration_type>` is one of the declaration types offered by Wasp (`app`, `route`, ...)
- `<declaration_name>` is an identifier chosen by you to name this specific declaration
- `<declaration_body>` is the value/definition of the declaration itself, which has to match the specific declaration body type determined by the chosen declaration type.

So, for `app` declaration above, we have:
- declaration type `app`
- declaration name `MyApp` (we could have used any other identifier, like `foobar`, `foo_bar`, or `hi3Ho`)
- declaration body `{ title: "My app" }`, which is a dictionary with field `title` that has string value.
  Type of this dictionary is in line with the declaration body type of the `app` declaration type.
  If we provided something else, e.g. changed `title` to `little`, we would get a type error from Wasp compiler since that does not match the expected type of the declaration body for `app`.

Each declaration has a meaning behind it that describes how your web app should behave and function.

All the other types in Wasp language (primitive types (`string`, `number`), composite types (`dict`, `list`), enum types (`DbSystem`), ...) are used to define the declaration bodies.

## Complete list of Wasp types
Wasp's type system can be divided into two main categories of types: **fundamental types** and **domain types**.

While fundamental types are here to be basic building blocks of a language, and are very similar to what you would see in other popular languages, domain types are what makes Wasp special, as they model the concepts of a web app like `page`, `route` and similar.

- Fundamental types ([source of truth](https://github.com/wasp-lang/wasp/blob/main/waspc/src/Wasp/Analyzer/Type.hs))
  - Primitive types
    - **string** (`"foo"`, `"they said: \"hi\""`)
    - **bool** (`true`, `false`)
    - **number** (`12`, `14.5`)
    - **declaration reference** (name of existing declaration: `TaskPage`, `updateTask`)
    - **ServerImport** (external server import) (`import Foo from "@server/bar.js"`, `import { Smth } from "@server/a/b.js"`)
      - The path has to start with "@server". The rest is relative to the `src/server` directory.
      - import has to be a default import `import Foo` or a single named import `import { Foo }`.
    - **ClientImport** (external client import) (`import Foo from "@client/bar.js"`, `import { Smth } from "@client/a/b.js"`)
      - The path has to start with "@client". The rest is relative to the `src/client` directory.
      - import has to be a default import `import Foo` or a single named import `import { Foo }`.
    - **json** (`{=json { a: 5, b: ["hi"] } json=}`)
    - **psl** (Prisma Schema Language) (`{=psl <psl data model syntax> psl=}`)
      - Check [Prisma docs](https://www.prisma.io/docs/concepts/components/prisma-schema/data-model) for the syntax of psl data model.
  - Composite types
    - **dict** (dictionary) (`{ a: 5, b: "foo" }`)
    - **list** (`[1, 2, 3]`)
    - **tuple** (`(1, "bar")`, `(2, 4, true)`)
      - Tuples can be of size 2, 3 and 4.
- Domain types ([source of truth](https://github.com/wasp-lang/wasp/blob/main/waspc/src/Wasp/Analyzer/StdTypeDefinitions.hs))
  - Declaration types
    - **action**
    - **api**
    - **apiNamespace**
    - **app**
    - **entity**
    - **job**
    - **page**
    - **query**
    - **route**
  - Enum types
    - **DbSystem**
    - **HttpMethod**
    - **JobExecutor**

For more details about each of the domain types, both regarding their body types and what they mean, check the [Features](/language/features.md) section.
