# @waspello/todo-module

A sample Wasp module that adds todo CRUD, APIs, and a React UI page.

## Install

```sh
npm install @waspello/todo-module
```

## Prisma requirements

Your schema must include a todo entity with at least:

```prisma
model TodoItem {
  id     Int     @id @default(autoincrement())
  text   String
  isDone Boolean @default(false)
}
```

For user scoping, add a relation to your User entity:

```prisma
model TodoItem {
  id     Int     @id @default(autoincrement())
  text   String
  isDone Boolean @default(false)
  user   User    @relation(fields: [userId], references: [id])
  userId Int
}
```

## Usage

Without auth (public access):

```ts
app.use(
  createTodoModule({
    todoEntityName: "TodoItem",
    route: "/todos",
  }),
);
```

With auth (scoped to current user):

```ts
app.use(
  createTodoModule({
    todoEntityName: "TodoItem",
    route: "/todos",
    cleanDoneTodosCron: "0 * * * *",
    userEntityName: "User",
    userForeignKey: "userId",
  }),
);
```

## Local registry testing

The module is normally consumed via `file:` link in waspello's `package.json`.
To test it as a real registry-published package (e.g. to verify peer dependency
resolution), you can publish it to a local [Verdaccio](https://verdaccio.org/)
instance.

### Prerequisites

```sh
npm install -g verdaccio@6 npm-cli-login@1
```

### Publish to local registry

```sh
# From this directory (packages/todo-module):
npm run local-registry
```

This starts Verdaccio on `http://localhost:4873`, publishes `wasp-config` and
`@waspello/todo-module` to it, and keeps the registry running.

### Install from local registry

```sh
# In examples/waspello:
npm install @waspello/todo-module@1.0.0 --registry http://localhost:4873
```

This installs the module from the registry instead of via `file:` link, which
means npm will fully enforce peer dependency ranges (just like a real publish).

### Stop the registry

```sh
npm run local-registry -- stop
```
