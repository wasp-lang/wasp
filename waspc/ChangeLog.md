# Changelog

## 0.11.8

### 🎉 [New Feature] Serving the Client From a Subdirectory

You can now serve the client from a subdirectory. This is useful if you want to serve the client from a subdirectory of your domain, e.g. `https://example.com/my-app/`.

To do this, you need to add the `client.baseDir` property to your `.wasp` file:

```wasp
app todoApp {
  // ...
  client: {
    baseDir: "/my-app",
  },
}
```

## 0.11.7

### 🐞 Bug fixes / 🔧 small improvements
- Fixed a bug with Prisma which prevent connections via SSL with our versions of Alpine and OpenSSL. We upgraded to the latest Prisma 4.X.X which fixes this issue.


## 0.11.6

### 🎉 [New Feature] Enable Customising the Vite Config

You can now customise the Vite config for your client app. This allows you to add plugins, change the dev server settings and more.

By adding a `vite.config.ts` or `vite.config.js` to your `client` directory, you can customise the Vite config. For example, you change the dev server behaviour
not to open the browser automatically:

```ts
import { defineConfig } from 'vite'

export default defineConfig({
  server: {
    open: false,
  },
})
```

⚠️ Be careful when changing the dev server port, you'll need to update the `WASP_WEB_CLIENT_URL` env var in your `.env.server` file.

### 🚧 [Experimental Feature] Wasp Studio

Running `wasp studio` in the root of your project starts Wasp Studio which visualises your application and shows you the relationships between pieces of your app. It is an experimental feature which is not yet fully ready, but we are working on it and will be adding more features to it in the future.

## 0.11.5

### 🐞 Bug fixes / 🔧 small improvements
- Fixed a bug in Auth UI imports that prevented users from using the social login buttons.

## 0.11.4

### 🎉 [New Feature] Signup Fields Customization

We added an API for extending the default signup form with custom fields. This allows you to add fields like `age`, `address`, etc. to your signup form.

You first need to define the `auth.signup.additionalFields` property in your `.wasp` file:
```wasp
app crudTesting {
  // ...
  auth: {
    userEntity: User,
    methods: {
      usernameAndPassword: {},
    },
    onAuthFailedRedirectTo: "/login",
    signup: {
      additionalFields: import { fields } from "@server/auth.js",
    },
  },
}
```

Then, you need to define the `fields` object in your `auth.js` file:
```js
import { defineAdditionalSignupFields } from '@wasp/auth/index.js'

export const fields = defineAdditionalSignupFields({
  address: (data) => {
    // Validate the address field
    if (typeof data.address !== 'string') {
      throw new Error('Address is required.')
    }
    if (data.address.length < 10) {
      throw new Error('Address must be at least 10 characters long.')
    }
    // Return the address field
    return data.address
  },
})
```

Finally, you can extend the `SignupForm` component on the client:
```jsx
import { SignupForm } from "@wasp/auth/forms/Signup";

export const SignupPage = () => {
  return (
    <div className="container">
      <main>
        <h1>Signup</h1>
        <SignupForm
          additionalFields={[
            {
              name: "address",
              label: "Address",
              type: "input",
              validations: {
                required: "Address is required",
              },
            },
          ]}
        />
      </main>
    </div>
  );
};
```
### 🎉 [New Feature] Support for PostgreSQL Extensions

Wasp now supports PostgreSQL extensions! You can enable them in your `main.wasp` file:

```wasp
app todoApp {
  // ...
  db: {
    system: PostgreSQL,
    prisma: {
      clientPreviewFeatures: ["postgresqlExtensions"],
      dbExtensions: [{
        name: "pgvector",
        // map: "vector", (optional)
        // schema: "public", (optional)
        // version: "0.1.0", (optiona)
      }]
    }
  }
}
```

This will add the necessary Prisma configuration to your `schema.prisma` file. Keep in mind that your database needs to support the extension you want to use. For example, if you want to use the `pgvector` extension, you need to install it in your database first.

### 🎉 [New Feature] Added Typescript support for Jobs

Now you can type your async jobs better and receive all the benefits of Typescript. When you define a job, Wasp will generate a generic type which you can use to type your job function:

```wasp
job simplePrintJob {
  executor: PgBoss,
  perform: {
    fn: import { simplePrint } from "@server/jobs.js",
  },
  entities: [Task]
}
```

```typescript
import { SimplePrintJob } from "@wasp/jobs/simplePrintJob";
import { Task } from "@wasp/entities";

export const simplePrint: SimplePrintJob<
  { name: string },
  { tasks: Task[] }
> = async (args, context) => {
  //        👆 args are typed e.g. { name: string }
  //                👆 context is typed e.g. { entitites: { Task: ... } }
  const tasks = await context.entities.Task.findMany({});
  return {
    tasks,
  };
};
```

When you use the job, you can pass the arguments and receive the result with the correct types:

```typescript
import { simplePrintJob } from "@wasp/jobs/simplePrintJob.js";

...
const job = await simplePrintJob.submit({ name: "John" })
...
const result = await result.pgBoss.details()
//      👆 result is typed e.g. { tasks: Task[] }
```

## 0.11.3

### 🎉 [New Feature] Type-safe links

Wasp now offers a way to link to pages in your app in a type-safe way. This means that you can't accidentally link to a page that doesn't exist, or pass the wrong arguments to a page.

After you defined your routes:

```wasp
route TaskRoute { path: "/task/:id", to: TaskPage }
```

You can get the benefits of type-safe links by using the `Link` component from `@wasp/router`:

```jsx
import { Link } from '@wasp/router'

export const TaskList = () => {
  // ...

  return (
    <div>
      {tasks.map((task) => (
        <Link
          key={task.id}
          to="/task/:id"
      {/* 👆 You must provide a valid path here */} 
          params={{ id: task.id }}>
      {/* 👆 All the params must be correctly passed in */}   
          {task.description}
        </Link>
      ))}
    </div>
  )
}
```

You can also get all the pages in your app with the `routes` object:

```jsx
import { routes } from '@wasp/router'

const linkToTask = routes.TaskRoute({ params: { id: 1 } })
```

### 🐞 Bug fixes
- Fixes API types exports for TypeScript users.
- Default .gitignore that comes with new Wasp project (`wasp new`) is now more aggressive when ignoring .env files, ensuring they don't get committed by accident (wrong name, wrong location, ...).


## 0.11.2

### 🎉 [New Feature] waspls Code Scaffolding

When an external import is missing its implementation, waspls now offers a Code Action to quickly scaffold the missing JavaScript or TypeScript function:

```wasp
query getTasks {
  fn: import { getTasks } from "@server/queries.js",
  //  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  //  ERROR: `getTasks` is not exported from `src/server/queries.ts`
  entities: [Task],
}
```

Using the code action (pressing <kbd>Ctrl</kbd> + <kbd>.</kbd> or clicking the lightbulb 💡 icon in VSCode) will add the following code to `src/server/queries.ts`:

```ts
import { GetTasks } from '@wasp/queries/types'

import GetTasksInput = void
import GetTasksOutput = void

export const getTasks: GetTasks<GetTasksInput, GetTasksOutput> = async (args, context) => {
  // Implementation goes here
}
```

### 🐞 Bug fixes / 🔧 small improvements
- Wasp copied over the `.env.server` instead of `.env.client` to the client app `.env` file. This prevented using the `.env.client` file in the client app.
- waspls thought that importing `"@client/file.jsx"` could mean `"@client/file.tsx"`, which could hide some missing import diagnostics and cause go-to definition to jump to the wrong file.


## 0.11.1

### 🎉 [New feature] Prisma client preview flags 
Wasp now allows you to enable desired `previewFeatures` for the Prisma client:
```
app MyApp {
  title: "My app",
  // ...
  db: {
    // ...
    prisma: {
      clientPreviewFeatures: ["extendedWhereUnique"]
    }
  }
}
```
Read all about Prisma preview features in [the official docs](https://www.prisma.io/docs/concepts/components/preview-features/client-preview-features).

## v0.11.0

### 🎉 Big new features 🎉 
- Automatic CRUD backend generation
- Public folder support
- Type safe WebSocket support
- Go to definition for imports in Wasp file

Check below for details on each of them!

### ⚠️ Breaking changes
- Wasp's **signup action** `import signup from '@wasp/auth/signup` now accepts only the user entity fields relevant to the auth process (e.g. `username` and `password`).
  This ensures no unexpected data can be inserted into the database during signup, but it also means you can't any more set any user entity fields via signup action (e.g. `age` or `address`).
  Instead, those should be set in the separate step after signup, or via a custom signup action of your own.
- Wasp now uses **React 18**! Check the following upgrade guide for details: https://react.dev/blog/2022/03/08/react-18-upgrade-guide .
  The most obvious difference you might notice is that your `useEffect` hooks run twice on component mount.
  This is due to the React 18's StrictMode, and it happens only during development, so it doesn't change the behaviour of your app in production.
  For more details on StrictMode, check https://react.dev/reference/react/StrictMode .
- Updated most of the npm dependencies that Wasp app is generated with (e.g. axios), so you will also need to update those that both you and Wasp use.
  Wasp will inform you about this with a warning/error message during compilation so just follow instructions.

### 🎉 [New feature] Public directory support
Wasp now supports a `public` directory in the `client` directory!

```
main.wasp
src/
├── client/
|   ├── public/  # <-- NEW!
|   |   ├── favicon.ico
|   |   └── robots.txt
|   └── ...
└── ...
```

All the files in this directory will be copied as they are to the `public` directory in the build folder.
This is useful for adding static assets to your project, like favicons, robots.txt, etc.

### 🎉 [New feature] Type safe WebSocket support

Wasp now supports WebSockets! This will allow you to have a persistent, realtime connection between your client and server, which is great for chat apps, games, and more.
What's more, Wasp's WebSockets support full-stack type safety, so you can be sure that your client and server are communicating with strongly typed events.

Enable WebSockets in your project by adding the following to your `main.wasp` file:

```
app todoApp {
  // ...

  webSocket: {
    fn: import { webSocketFn } from "@server/webSocket.js",
    autoConnect: true, // optional, default: true
  },
}
```

Then implement it on the server with optional types:
```typescript
import type { WebSocketDefinition } from '@wasp/webSocket'

export const webSocketFn: WebSocketFn = (io, context) => {
  io.on('connection', (socket) => {
    // ...
  })
}

type WebSocketFn = WebSocketDefinition<
  ClientToServerEvents,
  ServerToClientEvents
>

interface ServerToClientEvents {
  chatMessage: (msg: { id: string, username: string, text: string }) => void;
}

interface ClientToServerEvents {
  chatMessage: (msg: string) => void;
}
```

And use it on the client with automatic type inference:
```typescript
import React, { useState } from 'react'
import {
  useSocket,
  useSocketListener,
  ServerToClientPayload,
} from '@wasp/webSocket'

export const ChatPage = () => {
  const [messageText, setMessageText] = useState<
    // We are using a helper type to get the payload type for the "chatMessage" event.
    ClientToServerPayload<'chatMessage'>
  >('')
  const [messages, setMessages] = useState<
    ServerToClientPayload<'chatMessage'>[]
  >([])
  // The "socket" instance is typed with the types you defined on the server.
  const { socket, isConnected } = useSocket()

  // This is a type-safe event handler: "chatMessage" event and its payload type
  // are defined on the server.
  useSocketListener('chatMessage', logMessage)

  function logMessage(msg: ServerToClientPayload<'chatMessage'>) {
    setMessages((priorMessages) => [msg, ...priorMessages])
  }

  function handleSubmit(e: React.FormEvent<HTMLFormElement>) {
    e.preventDefault()
    // This is a type-safe event emitter: "chatMessage" event and its payload type
    // are defined on the server.
    socket.emit('chatMessage', messageText)
    // ...
  }

  // ...
}
```

### 🎉 [New feature] Automatic CRUD backend generation
You can tell Wasp to automatically generate server-side logic (Queries and Actions) for creating, reading, updating, and deleting a specific entity. As you change that entity, Wasp automatically regenerates the backend logic.

Example of a `Task` entity with automatic CRUD:

```
crud Tasks {
  entity: Task,
  operations: {
    getAll: {
      isPublic: true, // by default only logged in users can perform operations
    },
    get: {},
    create: {
      overrideFn: import { createTask } from "@server/tasks.js",
    },
    update: {},
    delete: {},
  },
}
```

This gives us the following operations: `getAll`, `get`, `create`, `update` and `delete`, which we can use in our client like this:

```typescript
import { Tasks } from '@wasp/crud/Tasks'
import { useState } from 'react'

export const MainPage = () => {
  const { data: tasks, isLoading, error } = Tasks.getAll.useQuery()
  const createTask = Tasks.create.useAction()
  // ...

  function handleCreateTask() {
    createTask({ description: taskDescription, isDone: false })
    setTaskDescription('')
  }

  // ...
}
```

### 🎉 [New feature] IDE tooling improvements

#### Go to definition from wasp file + detection of invalid imports

```
query getRecipes {
  fn: import { getRecipes } from "@server/recipe.js",  // <- You can now click on this import!
  entities: [Recipe],
}
```

Wasp language server just got smarter regarding imports in wasp file!
1. If there is no file to which import points, error is reported.
2. If file doesn't contain symbol that we are importing, error is reported.
3. Clicking on import statement now takes you to the code that is being imported.

We have more ideas in this direction on the way though!
A bit of a sneak peek of what is coming soon: if Wasp recognizes file / symbol is missing, it will offer to scaffold the code for you!

#### Autocompletion for dictionary keys

```
app RecipeApp {
  title: "My Recipes",
  wasp: { version: "^0.10.0" },
  auth: {
    methods: { usernameAndPassword: {} },
    █       // <- your cursor
  }
}
```

As per popular request, Wasp language server now recognizes when you are in dictionary and will offer possible key values for autocompletion!
For instance, in the code example above, it will offer completions such as `onAuthFailedRedirectTo`, `userEntity`, ... .
It will also show their types.

### 🐞 Bug fixes / 🔧 small improvements
- Wasp now uses TypeScript to ensure all payloads sent to or from operations (queries and actions) are serializable.
- Wasp starter templates now show description.
- Wasp CLI now correctly exits with exit code 1 after compiler bug crash.
- Added extra type info to middleware customization fn.
- Upgraded most of the dependencies (with react-router and prisma upgrades coming soon).
- Wasp CLI now always shows a nice error message when database is not accessible.
- We now ensure that User entity's username field must have `unique` attribute.
- Improved how Wasp CLI detects wrong/missing node + the error message it prints.


## v0.10.6

### Bug fixes
- `wasp deploy fly launch` now supports the latest `flyctl launch` toml file for the web client (which changed their default structure and port).

### More `wasp deploy fly` options
`wasp deploy fly` now supports a `--org` option, as well as setting secrets during `launch`.

## v0.10.5

### Bug fixes
- Wasp CLI will now forward error exit codes. This will help when used in scripted contexts.
- Wasp now renders only the first route that matches the current path in the browser. 

### Express middleware customization
We now offer the ability to customize Express middleware:
- globally (impacting all actions, queries, and apis by default)
- on a per-api basis
- on a per-path basis (groups of apis)


### Interactive new project creation
We now offer an interactive way to create a new project. You can run `wasp new` and follow the prompts to create a new project. This is the recommended way to create a new project. It will ask you for the project name and to choose one of the starter templates.

## v0.10.4

### Bug fixes
- Adds missing import for HttpError which prevent auth from working properly.

## v0.10.3
- Fixed a bug with circular imports in JS code which prevented database seeding from working properly.

## v0.10.2

### Bug fixes
- Fixed a bug where JS arrays weren't generated properly from Haskell code which caused issues with oAuth, operations and cache invalidation.

## v0.10.1

### Bug fixes
- Fixed several TypeScript errors preventing the frontend build

## v0.10.0

### Breaking changes

- We changed `LoginForm` and `SignupForm` to use a named export instead of a default export, this means you must use them like this:
    - `import { LoginForm } from '@wasp/auth/forms/Login'`
    - `import { SignupForm } from '@wasp/auth/Signup'`
- We changed some of the extensions on Wasp-provided imports from `.js` to `.ts`. For example `useAuth.js` is now `useAuth.ts`. Therefore, you should import them like this: `import useAuth from '@wasp/auth/useAuth'` (without the `.js` extension). Some other affected imports are `@wasp/auth/login.js`, `@wasp/auth/logout.js`, and similar.
- We changed the type arguments for `useQuery` and `useAction` hooks. They now take two arguments (the `Error` type argument was removed):
  - `Input` - This type argument specifies the type for the **request's payload**.
  - `Output` - This type argument specifies the type for the **resposne's payload**.

### Full-stack type safety for Operations
Frontend code can now infer correct payload/response types for Queries and Actions from their definitions on the server.

Define a Query on the server:
```typescript
export const getTask: GetTaskInfo<Pick<Task, "id">, Task> = 
  async ({ id }, context) => {
    // ...
  }
```

Get properly typed functions and data on the frontend:
```typescript
import { useQuery } from "@wasp/queries"
// Wasp knows the type of `getTask` thanks to your backend definition.
import getTask from "@wasp/queries/getTask"

export const TaskInfo = () => {
  const {
    // TypeScript knows `task` is a `Task | undefined` thanks to the
    // backend definition.
    data: task,
    // TypeScript knows `isError` is a `boolean`.
    isError,
    // TypeScript knows `error` is of type `Error`.
    error,
    // TypeScript knows the second argument must be a `Pick<Task, "id">` thanks
    // to the backend definition.
  } = useQuery(getTask, { id: 1 })

  if (isError) {
    return <div> Error during fetching tasks: {error.message || "unknown"}</div>
  }

  // TypeScript forces you to perform this check.
  return taskInfo === undefined ? (
    <div>Waiting for info...</div>
  ) : (
    <div>{taskInfo}</div>
  )
}
```
The same feature is available for Actions.

### Payloads compatible with Superjson
Client and the server can now communicate with richer payloads.

Return a Superjson-compatible object from your Operation:
```typescript
type FooInfo = { foos: Foo[], message: string, queriedAt: Date }

const getFoos: GetFoo<void, FooInfo> = (_args, context) => {
  const foos = context.entities.Foo.findMany()
  return {
    foos,
    message: "Here are some foos!",
    queriedAt: new Date(),
  }
}
```
And seamlessly use it on the frontend:

```typescript
import getfoos from "@wasp/queries/getTask"

const { data } = useQuery(getfoos)
const { foos, message, queriedAt } = data
// foos: Foo[]
// message: string
// queriedAt: Date
```

### E-mail authentication

You can now use e-mail authentication in your Wasp app! This means that users can sign up and log in using their e-mail address. You get e-mail verification and password reset out of the box.

```c
app MyApp {
  // ...
  auth: {
    // ...
    email: {
        fromField: {
          name: "ToDO App",
          email: "hello@itsme.com"
        },
        emailVerification: {
          getEmailContentFn: import { getVerificationEmailContent } from "@server/auth/email.js",
          clientRoute: EmailVerificationRoute,
        },
        passwordReset: {
          getEmailContentFn: import { getPasswordResetEmailContent } from "@server/auth/email.js",
          clientRoute: PasswordResetRoute
        },
        allowUnverifiedLogin: false,
      },
  }
}
```

You can only use one of e-mail or username & password authentication in your app. You can't use both at the same time.

### Auth UI components

Wasp now provides a set of UI components for authentication. You can use them to quickly build a login and signup page for your app. The UI changes dynamically based on your Wasp config.

We provide `LoginForm`, `SignupForm`, `ForgotPassworForm`, `ResetPasswordForm` and`VerifyEmailForm` components. You can import them from `@wasp/auth/forms` like:

```js
import { LoginForm } from '@wasp/auth/forms/Login'
import { SignupForm } from '@wasp/auth/forms/Signup'
import { ForgotPasswordForm } from '@wasp/auth/forms/ForgotPassword'
import { ResetPasswordForm } from '@wasp/auth/forms/ResetPassword'
import { VerifyEmailForm } from '@wasp/auth/forms/VerifyEmail'
```

### Database seeding 
You can now define JS/TS functions for seeding the database!

```c
app MyApp {
  // ...
  db: {
    seeds: [
      import { devSeedSimple } from "@server/dbSeeds.js",
      import { prodSeed } from "@server/dbSeeds.js",
    ]
  }
}
```

```js
import { createTask } from './actions.js'

export const devSeedSimple = async (prismaClient) => {
  const { password, ...newUser } = await prismaClient.user.create({
    username: "RiuTheDog", password: "bark1234"
  })
  await createTask(
    { description: "Chase the cat" },
    { user: newUser, entities: { Task: prismaClient.task } }
  )
}

//...
```

Run `wasp db seed` to run database seeding. If there is only one seed, it will run that one, or it will interactively ask you to pick one.
You can also do `wasp db seed <name>` to run a seed with specific name: for example, for the case above, you could do `wasp db seed prodSeed`.


### The `api` keyword for defining an arbitrary endpoint and URL
Need a specific endpoint, like `/healthcheck` or `/foo/callback`? Or need complete control of the response? Use an `api` to define one by tying a JS function to any HTTP method and path! For example:
```ts
// main.wasp
api fooBar {
  fn: import { foo } from "@server/apis.js",
  entities: [Task],
  httpRoute: (GET, "/foo/callback")
}

// server/api.ts
import { FooBar } from '@wasp/apis/types'

export const fooBar : FooBar = (req, res, context) => {
  res.set('Access-Control-Allow-Origin', '*') // Example of modifying headers to override Wasp default CORS middleware.
  res.json({ msg: `Hello, ${context.user?.username || "stranger"}!` })
}
```

### E-mail sending support

Wasp now supports sending e-mails! You can use the `emailSender` app property to configure the e-mail provider and optionally the `defaultFrom` address. Then, you can use the `send` function in your backend code to send e-mails.

```ts
// main.wasp
app MyApp {
  emailSender: {
    provider: SendGrid,
    defaultFrom: {
      name: "My App",
      email: "myapp@domain.com"
    },
  },
}

// server/actions.ts
import { emailSender } from '@wasp/email/index.js'

// In some action handler...
const info = await emailSender.send({
    to: 'user@domain.com',
    subject: 'Saying hello',
    text: 'Hello world',
    html: 'Hello <strong>world</strong>'
})
```

### `wasp start db` -> Wasp can now run your dev database for you with a single command

Moving from SQLite to PostgreSQL with Wasp can feel like increase in complexity, because suddenly you have to care about running your PostgreSQL database, providing connection URL for it via env var, and if you checkout somebody's else Wasp project, or your old Wasp project that you have no memory of any more, you also have to figure all that out.

To help with that, we now added `wasp start db`, which runs a development database for you!
That it, all you need to do is run `wasp start db` and you are good to go. No env var setting, no remembering how to run the db.

NOTE: Requires `docker` to be installed and in `PATH`, and docker daemon to be running.

### `wasp test client` -> Wasp can now test your web app code
By leveraging Vitest and some supporting libraries, Wasp now makes it super easy to add unit tests and React component tests to your frontend codebase.

### `pg-boss` upgraded to latest version (8.4.2)
This `pg-boss` release fixes an issue where the node server would exit due to an unhandled exception when the DB connection was lost.

### Bug fixes
- Starts the process of removing the coupling between `usernameAndPassword` and social logins. Now, your `userEntity` no longer requires a `username` or `password` field if you only want to use Google/GitHub for auth.

## v0.9.0

### BREAKING CHANGES
- All client files which use `JSX` need to have either the `.jsx` or the `.tsx` extension. This is because we now use `Vite` under the hood instead of `Create React App`, and `Vite` requires these extensions to be present to process `JSX`` properly.
- The Tailwind and PostCSS config files need to have the `.cjs` extension. These config files are CommonJS modules, and with `Vite` we are using ES modules by default.

### Wasp now uses Vite instead of Create React App
We moved away from using Create React App for the client app. This means that dev startup time will be much faster and we are following the latest best practices for building web apps with React.

### Express `app` and http `server` available in server `setupFn`
Wasp now passes in a context to the server `setupFn` that contains Express `app` and http `server` objects. This can be used as an escape hatch for things like custom routes or WebSocket support.

## v0.8.2

### Non-breaking Changes
- The Dockerfile has been updated to build the server files during the Docker build stage instead of during server startup. This will reduce the memory footprint required for running apps.

### Bug fixes
- Fixes a file lock error that kills CLI when changing entities with `wasp start` running on newer Macs.

### Support for defining the web app's root component
You can now define a root component for your client app. This is useful if you want to wrap your app in a provider or have a common layout. You can define it in `app.client.rootComponent` in your `.wasp` file.

### `wasp deploy` CLI command added
We have made it much easier to deploy your Wasp apps via a new CLI command, `wasp deploy`. 🚀 This release adds support for Fly.io, but we hope to add more hosting providers soon!

### Import Wasp Entity types (on frontend and backend)
You can now import and use the types of Wasp entities anywhere in your code.

Let's assume your Wasp file contains the following entity:
```c
entity Task {=psl
    id          Int     @id @default(autoincrement())
    description String
    isDone      Boolean @default(false)
    user        User    @relation(fields: [userId], references: [id])
    userId      Int
psl=}
```
Here's how you can access and use its type in a backend file:
```typescript
import { Task } from '@wasp/entities/Task'

const getTasks = (args, context) => {
    const tasks: Task[] = // ...
    // ...
}
```
And here's how you can to the same in a frontend file:

```typescript
// ...
import { useQuery } from '@wasp/queries'
import getTasks from '@wasp/queries/getTasks.js'
import { Task } from '@wasp/entities'

type TaskPayload = Pick<Task, "id">

const Todo = (props: any) => {
  // The variable 'task' will now have the type Task.
  const { data: task } = useQuery<TaskPayload, Task>(getTask, { id: taskId })
  // ...
}

```

### Automatically generated types for Queries and Actions
Wasp now automatically generates appropriate types for the operations specified
in your `.wasp` file. This reduces duplication and eliminates possible errors
(i.e., no way to specify incorrect entities). Assuming your `.wasp` file looks
like this:
```c
query getTasks {
  fn: import { getTasks } from "@server/queries.js",
  entities: [Task]
}
```
You'll get the following feature:
```typescript
import { Task } from '@wasp/entities'
import { GetTasks} from '@wasp/queries'

type Payload = Pick<Task, 'isDone'>;

// Use the type parameters to specify the Query's argument and return types.
const getTasks: GetTasks<Payload, Task[]> = (args, context) => {
  // Thanks to the definition in your `.wasp` file, the compiler knows the type
  // of `context` (and that it contains the `Task` entity).
  //
  // Thanks to the first type argument in `GetTasks`, the compiler knows `args`
  // is of type `Payload`.
  //
  // Thanks to the second type argument in `GetTasks`, the compiler knows the
  // function must return a value of type `Task[]`.
}
```

### Uninstall command
If you want to uninstall Wasp from your system, you can now do so with:
```bash
wasp uninstall
```
It will remove all of the Wasp binaries and data from your system.

## v0.8.1

### Remove npm version constraint
We are removing the requirement for a specific npm version to enable following the Node.js LTS releases (Node.js LTS releases sometimes bump the major `npm` version).
We are still requiring Node.js to be version 18, but the `npm` version can be anything and for most of Wasp users it will be the version that comes with Node.js.

## v0.8.0

### BREAKING CHANGES
- Social auth had several breaking changes as we added a new provider (GitHub).
  - Buttons and sign in URLs now have a different, standardized import name for each provider.
    - Google exe: `import { SignInButton as GoogleSignInButton, signInUrl, logoUrl } from '@wasp/auth/buttons/Google'`
  - Buttons themselves have been restyled to make them more uniform, and no longer take an optional `height` parameter.
  - Social config object now use a `clientID` property instead of `clientId`.

### GitHub added as a social login
We have added GitHub as another social login option. It is as easy to use as Google, and only requires adding `gitHub` to your `app.auth.methods` plus two environment variables (`GITHUB_CLIENT_ID` and `GITHUB_CLIENT_SECRET`)! Check out the docs for more.

## v0.7.3

### MINOR CLI BREAKING CHANGE
- The CLI command for applying a migration with a name has changed from `wasp db migrate-dev foo` to `wasp db migrate-dev --name foo`. This allowed us to add more flags, like `--create-only`.

### Bug fixes
- Again fixed Dockerfile generated with `wasp build` (after fixing it only half-way last time :facepalm) -> Prisma would break due to unsupported version of openssl.

## v0.7.2

### Bug fixes
- Fixed Dockerfile generated with `wasp build` -> Prisma would break due to unsupported version of openssl.
  https://github.com/wasp-lang/wasp/issues/877

## v0.7.1

### Bug fixes
- Fixed a bug that was causing Wasp to forget about compiling backend code before running it in production

## v0.7.0 - Beta Release!

### BREAKING CHANGES
- Updates Create React App from version 4.0.3 to 5.0.1. This brings many improvements as well as downstream library updates. It also has a list of possible breaking changes: https://github.com/facebook/create-react-app/blob/main/CHANGELOG.md
- Updates Prisma from version 3.15.2 to 4.5.0. Please check out their upgrade guide: https://www.prisma.io/docs/guides/upgrade-guides/upgrading-versions/upgrading-to-prisma-4 and release notes: https://github.com/prisma/prisma/releases for any possible breaking changes.
- Removes default `index.css` file that provided basic `body` defaults. Now, there is no default CSS applied.
- Updates required Node LTS version from version 16 to version 18. This Node ecosystem change happened on 2022-10-25: https://github.com/nodejs/Release

#### Significant changes to Wasp project structure
This was the file tree of a newly generated project in the previous version of Wasp
(i.e., this was what you used to get by running `wasp new project`):
```
.
├── ext
│   ├── Main.css
│   ├── MainPage.js
│   ├── .waspignore
│   └── waspLogo.png
├── .gitignore
├── main.wasp
└── .wasproot
```
This is the file tree of a newly generated project in the newest release of Wasp (i.e., this is what you will
get by running `wasp new project` from this point onwards):
```
.
├── .gitignore
├── main.wasp
├── src
│   ├── client
│   │   ├── Main.css
│   │   ├── MainPage.jsx
│   │   ├── react-app-env.d.ts
│   │   ├── tsconfig.json
│   │   └── waspLogo.png
│   ├── server
│   │   └── tsconfig.json
│   ├── shared
│   │   └── tsconfig.json
│   └── .waspignore
└── .wasproot
```

Main differences:
- All server-side code must be located inside the `src/server` directory.  Wasp
declarations must import this code with `import foo from "@server/foo.js"`
(instead of `import foo from "@ext/foo.js"`)
- All client-side code must be located inside the `src/client` directory.  Wasp
declarations must import this code with `import foo from "@client/bar.js"`
(instead of `import bar from "@ext/bar.js"`)
- All shared code (i.e., used on both the client and the server) must be
located inside the `src/shared` and imported where needed through a relative import.
- Each of these subdirectories (i.e., `src/server`, `src/client`, and
`src/shared`) comes with a pregenerated `tsconfig.json` file. This file helps
with IDE support (e.g., jumping to definitions, previewing types, etc.) and you
shouldn't delete it. The same goes for `react-app-env.d.ts`

The new structure is fully reflected in [our docs](https://wasp-lang.dev/docs/language/overview), but we'll also
provide a quick guide for migrating existing projects.

##### Migrating an existing Wasp project to the new structure

You can easily migrate your old Wasp project to the new structure by following a
series of steps. Assuming you have a project called `foo` inside the
directory `foo`, you should:
  1. Install the latest version of Wasp
  2. Rename your project's root directory to something like `foo_old`
  3. Create a new project by running `wasp new foo`
  4. Copy all server-side code from `foo_old/ext` to `foo/src/server`
  5. Copy all client-side code from `foo_old/ext` to `foo/src/client`
  6. Copy all shared code (if any) from `foo_old/ext` to `foo/src/shared` and
  adapt imports in files that reference it:
     - For example, `import bar from './bar.js'` becomes `import bar from "../shared/bar.js"`
  7. Copy all lines you might have added to `foo_old/.gitignore` into
  `foo/.gitignore`
  8. Finally, copy `foo_old/main.wasp` to `foo/main.wasp` and correct external
  imports:
      - Queries, Actions, Jobs, and the Server setup function must import their code from `@server`
      - Pages and the Client setup function must import their code from `@client`

     For example, if you previously had something like:
     ```js
     page LoginPage {
       // This previously resolved to ext/LoginPage.js
       component: import Login from "@ext/LoginPage.js"
     }

     // ...

     query getTasks {
       // This previously resolved to ext/queries.js
       fn: import { getTasks } from "@ext/queries.js",
     }
     ```

     You should change it to:

     ```js
     page LoginPage {
       // This resolves to src/client/LoginPage.js
       component: import Login from "@client/LoginPage"
     }

     // ...

     query getTasks {
       // This resolves to src/server/queries.js
       fn: import { getTasks } from "@server/queries.js",
     }
     ```
     Do this for all external imports in your `.wasp` file. After you're done, there shouldn't be any occurences of the string `"@ext"`.

That's it! You should now have a fully working Wasp project in the `foo` directory.

### [NEW FEATURE] TypeScript support

Wasp now allows you to write TS and TSX files. Some (but not all) Wasp features
come with type definitions. Except more type definitions and even better
integration with TypeScript in future versions!

### [NEW FEATURE] Dockerfile customization

You can now customize the default Wasp Dockerfile by either extending/replacing our build stages or using your own custom logic. To make use of this feature, simply add a Dockerfile to the root of your project and it will be appended to the bottom of the existing Wasp Dockerfile.

### [NEW FEATURE] Tailwind CSS support

You can now use the Tailwind CSS framework in your project by simply adding two config files. Check out the Integrations section of our Docs for more!

## v0.6.0.0 (2022/09/29)

### BREAKING CHANGES
- The `EmailAndPassword` auth method has been renamed `usernameAndPassword` to better reflect the current usage. Email validation will be addressed in the future.
  - This means the `auth.userEntity` model should now have field called `username` (instead of `email`, as before).
    - If you'd like to treat the old `email` field as `username`, you can create a migration file like so:
      ```bash
      $ cd migrations
      $ mkdir "migrations/`date -n +%Y%m%d%H%M%S`_some_name" && touch $_/migration.sql
      ```
      You can then add contents like the following:
      ```sql
        -- Drop the old index (NOTE: name may vary based on Prisma version)
      DROP INDEX "User_email_key";

      -- Alter the table to rename the column, thus preserving the data
      ALTER TABLE "User"
      RENAME COLUMN "email" TO "username";

      -- Create a new index
      CREATE UNIQUE INDEX "User_username_key" ON "User"("username");
      ```
      - NOTE: If you simply changed `email` to `username` in your .wasp file, Prisma will try to drop the table and recreate it, which is likely not what you want if you have data you want to preserve.
    - If you would like to add a new `username` column and keep `email` as is, be sure to add a calculated value in the migration (perhaps a random string, or something based on the `email`). The `username` column should remain `NOT NULL` and `UNIQUE`.
- `WASP_WEB_CLIENT_URL` is now a required environment variable to improve CORS security. It is set by default in development. In production, this should point to the URL where your frontend app is being hosted.
- The generated Dockerfile has been updated from `node:14-alpine` to `node:16-alpine`.
- Wasp Jobs callback function arguments have been updated to the following: `async function jobHandler(args, context)`. Jobs can now make use of entities, accessed via `context`, like Operations. Additionally, the data passed into the Job handler function are no longer wrapped in a `data` property, and are now instead accessed exactly as they are supplied via `args`.
- React got updated to React 17.

### [NEW FEATURE] Google is now a supported authentication method!

You can now offer your users the ability to sign in with Google! Enabling it is just a few lines and offers a fast, easy, and secure way to get users into your app! We also have a comprehensive setup guide for creating a new app in the Google Developer Console.

Stay tuned, as more external auth methods will be added in the future. Let us know what you'd like to see support for next!

### [NEW FEATURE] Wasp Language Server

Now, your installation of Wasp also brings Wasp language server with it! This means live error reporting in Wasp files in supported IDEs (currently only VSCode).

Make sure to update your Wasp VSCode extension to get the benefits of Wasp Language Server.

### [NEW FEATURE] Optimistic updates via useAction hook

We added `useAction` hook to our JS API, which allows you to specify optimistic update details for an Action.
This means that, if you have a good idea of how an Action will affect the state on the client, you can perform those changes immediatelly upon its call (instead of waiting for Action to finish), by modifying what specific Queries currently return.
Once Action is actually done, related Queries will be unvalidated as usual and therefore fetch the real result, but in the meantime the changes you specified via optimistic updates will be visible.

This is great for apps where there is a lot of interactivity and you want the UI to update instantly with your changes, even as they are still being saved to the server.

Check out https://wasp-lang.dev/docs/language/features#the-useaction-hook for more details.

### Bug fixes
- Works around a `sodium-native` bug (used by a Wasp dependency, `secure-password`) that caused signup/login runtime issues with Heroku deployments by downgrading it from v3.4.1 to v3.3.0 via a `package.json` override. Ref: https://github.com/sodium-friends/sodium-native/issues/160
- Improved warnings by Wasp to do database migration -> now there are less false positives.

---

## v0.5.2.1 (2022/07/14)

### Bug fixes
- Made wasp CLI more robust regarding encoding used on the machine.
- Worked around the bug in latest npm, so that Wasp now again supports latest LTS npm version.

---

## v0.5.2.0 (2022/06/23)

### Upgraded Prisma to latest version (13.15.2)

Among various other things, this brins support for OpenSSL3. So if you couldn't run Wasp on your operating system due to Prisma not supporting OpenSSL3, those days are over!

---

## v0.5.1.0 (2022/06/17)

### [NEW FEATURES]
- There is now `app.client.setup` function in .wasp that you can use to define custom setup you want to do on client before on its initialization.
- You can now configure the React Query's QueryClient by calling special function exposed by Wasp in your JS (in `app.client.setup`).

### Various improvements and bug fixes
- Limited Wasp node version to <=16.15.0 for now, since there is a problem with later versions and how Wasp uses `npx`.
- Reduced some of the redundant warning messages in Wasp CLI.
- Fixed unresponsive UI on server reload.

---

## v0.5.0.0 (2022/05/18)

### [NEW FEATURE] Wasp now has support for running Jobs!

If you have server tasks that you do not want to handle as part of the normal request-response cycle, now Wasp allows you to make that function a Job and it will gain some "superpowers"!

Jobs will persist between server restarts, can be retried if they fail, and they can even be delayed until the future (or have a recurring schedule)!

Some examples where you may want to use a Job on the server include sending an email, making an HTTP request to some external API, or doing some nightly calculations.

To run Jobs, you don't need any additional infrastructure at the moment, just a Postgre database that you anyway need to deploy Wasp to production.

### BREAKING CHANGES

- Wasp now requires latest LTS version of NodeJS
  - We had a bit of issues with being too relaxed on the version of NodeJS that can be used with Wasp so we thightened it up a bit.
    We also added a more thorough check in Wasp for it, that will warn you very explicitely if you are using the wrong version of Node.
- Updated react-query to v3
  - This brings some new features from react query while also laying the foundation for the further features we are building on top of it in Wasp (coming soon!).
- Updated python to python3 in Dockerfile generated upon `wasp build`.

### Various improvements

- Finally fixed a bug with orphaned processes in development.
- Various other bug fixes, doc improvements, and refactorings.

---

## v0.4.0.0 (2022/02/23)

### [BREAKING CHANGE] Upgrading Prisma to version 3.9.1

We are happy to announce Wasp is now using a much newer version of Prisma! This change does not impact the Wasp DSL support for Prisma, but it does come with some caveats from Prisma based on your usage. Please see this note for any breaking changes: https://www.prisma.io/docs/guides/upgrade-guides/upgrading-versions/upgrading-to-prisma-3

*Note: When you first migrate after upgrading, you will likely see a new migration created for 3.x specific features related to updating foreign keys and indexes.*

### Various improvements

- Automatically regenerating your Prisma client, as needed, based on your Prisma schema changes.
- Tracking your NPM project dependency changes and automatically invoking `npm install`, as needed, so you are always up to date.
- and more!

---

## v0.3.0.0 (2022/02/04)

### [BREAKING CHANGE] New Wasp-lang syntax!

Mostly it is very similar to what it was before, with some following bigger changes:
  - `auth`, `dependencies`, and couple of other "singleton" delcarations now became part of `app` declaration.
  - All declarations now need to have name, including `route`.
  - `route` has different syntax.
  - `dependencies` have different syntax.

For exact details about new syntax, check https://wasp-lang.dev/docs/language/syntax .

### Various improvements

  - Better compiler error messages.
  - Nicer CLI output.
  - Added delay on recompilation to avoid redundant recompiling.
  - Added `onAuthSucceededRedirectTo` field in `app`.
  - and more!

## Unreleased changes
