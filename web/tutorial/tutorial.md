RUN wasp new TodoApp
RUN cd TodoApp

SET `src/MainPage.tsx`
```tsx
export const MainPage = () => {
  return <div>Hello world!</div>
}
```

SET `main.wasp`
```wasp
app TodoApp {
  wasp: {
    version: "^0.16.0"
  },
  title: "TodoApp"
}

route RootRoute { path: "/", to: MainPage }
page MainPage {
  component: import { MainPage } from "@src/MainPage"
}
```

APPEND `schema.prisma`

```prisma
model Task {
    id          Int     @id @default(autoincrement())
    description String
    isDone      Boolean @default(false)
}
```

RUN wasp db migrate-dev

APPEND `main.wasp`

```wasp
query getTasks {
  // Specifies where the implementation for the query function is.
  // The path `@src/queries` resolves to `src/queries.ts`.
  // No need to specify an extension.
  fn: import { getTasks } from "@src/queries",
  // Tell Wasp that this query reads from the `Task` entity. Wasp will
  // automatically update the results of this query when tasks are modified.
  entities: [Task]
}
```

SET `src/queries.ts`

```ts
import { Task } from 'wasp/entities'
import { type GetTasks } from 'wasp/server/operations'

export const getTasks: GetTasks<void, Task[]> = async (args, context) => {
  return context.entities.Task.findMany({
    orderBy: { id: 'asc' },
  })
}
```

SET `src/MainPage.tsx`

```tsx
import { Task } from 'wasp/entities'
import { getTasks, useQuery } from 'wasp/client/operations'

export const MainPage = () => {
  const { data: tasks, isLoading, error } = useQuery(getTasks)

  return (
    <div>
      {tasks && <TasksList tasks={tasks} />}

      {isLoading && 'Loading...'}
      {error && 'Error: ' + error}
    </div>
  )
}

const TaskView = ({ task }: { task: Task }) => {
  return (
    <div>
      <input type="checkbox" id={String(task.id)} checked={task.isDone} />
      {task.description}
    </div>
  )
}

const TasksList = ({ tasks }: { tasks: Task[] }) => {
  if (!tasks?.length) return <div>No tasks</div>

  return (
    <div>
      {tasks.map((task, idx) => (
        <TaskView task={task} key={idx} />
      ))}
    </div>
  )
}
```

APPEND `main.wasp`

```wasp
action createTask {
  fn: import { createTask } from "@src/actions",
  entities: [Task]
}
```

SET `src/actions.ts`

```ts
import { Task } from 'wasp/entities'
import { CreateTask } from 'wasp/server/operations'

type CreateTaskPayload = Pick<Task, 'description'>

export const createTask: CreateTask<CreateTaskPayload, Task> = async (
  args,
  context
) => {
  return context.entities.Task.create({
    data: { description: args.description },
  })
}
```

APPEND `src/MainPage.tsx`

```tsx
import { FormEvent } from 'react'
import { Task } from 'wasp/entities'
import {
  createTask,
  getTasks,
  useQuery
} from 'wasp/client/operations'

// ... MainPage, TaskView, TaskList ...

const NewTaskForm = () => {
  const handleSubmit = async (event: FormEvent<HTMLFormElement>) => {
    event.preventDefault()
    try {
      const target = event.target as HTMLFormElement
      const description = target.description.value
      target.reset()
      await createTask({ description })
    } catch (err: any) {
      window.alert('Error: ' + err.message)
    }
  }

  return (
    <form onSubmit={handleSubmit}>
      <input name="description" type="text" defaultValue="" />
      <input type="submit" value="Create task" />
    </form>
  )
}
```

APPEND `src/MainPage.tsx`

```tsx
import { FormEvent } from 'react'
import { Task } from 'wasp/entities'
import {
  createTask,
  getTasks,
  useQuery
} from 'wasp/client/operations'

const MainPage = () => {
  const { data: tasks, isLoading, error } = useQuery(getTasks)

  return (
    <div>
      <NewTaskForm />

      {tasks && <TasksList tasks={tasks} />}

      {isLoading && 'Loading...'}
      {error && 'Error: ' + error}
    </div>
  )
}

// ... TaskList, TaskView, NewTaskForm ...
```

APPEND `main.wasp`

```wasp
action updateTask {
  fn: import { updateTask } from "@src/actions",
  entities: [Task]
}
```

APPEND `src/actions.ts`

```ts
import { CreateTask, UpdateTask } from 'wasp/server/operations'

// ...

type UpdateTaskPayload = Pick<Task, 'id' | 'isDone'>

export const updateTask: UpdateTask<UpdateTaskPayload, Task> = async (
  { id, isDone },
  context
) => {
  return context.entities.Task.update({
    where: { id },
    data: {
      isDone: isDone,
    },
  })
}
```

APPEND `src/MainPage.tsx`

```tsx
import { FormEvent, ChangeEvent } from 'react'
import { Task } from 'wasp/entities'
import {
  updateTask,
  createTask,
  getTasks,
  useQuery,
} from 'wasp/client/operations'


// ... MainPage ...

const TaskView = ({ task }: { task: Task }) => {
  const handleIsDoneChange = async (event: ChangeEvent<HTMLInputElement>) => {
    try {
      await updateTask({
        id: task.id,
        isDone: event.target.checked,
      })
    } catch (error: any) {
      window.alert('Error while updating task: ' + error.message)
    }
  }

  return (
    <div>
      <input
        type="checkbox"
        id={String(task.id)}
        checked={task.isDone}
        onChange={handleIsDoneChange}
      />
      {task.description}
    </div>
  )
}

// ... TaskList, NewTaskForm ...
```

APPEND `schema.prisma`

```prisma
model User {
  id Int @id @default(autoincrement())
}
```

APPEND `main.wasp`

```wasp
app TodoApp {
  wasp: {
    version: "^0.16.0"
  },
  title: "TodoApp",
  auth: {
    // Tells Wasp which entity to use for storing users.
    userEntity: User,
    methods: {
      // Enable username and password auth.
      usernameAndPassword: {}
    },
    // We'll see how this is used in a bit.
    onAuthFailedRedirectTo: "/login"
  }
}

// ...
```

RUN wasp db migrate-dev

APPEND `main.wasp`

```wasp
route SignupRoute { path: "/signup", to: SignupPage }
page SignupPage {
  component: import { SignupPage } from "@src/SignupPage"
}

route LoginRoute { path: "/login", to: LoginPage }
page LoginPage {
  component: import { LoginPage } from "@src/LoginPage"
}
```

SET `src/SignupPage.tsx`

```tsx
import { Link } from 'react-router-dom'
import { SignupForm } from 'wasp/client/auth'

export const SignupPage = () => {
  return (
    <div style={{ maxWidth: '400px', margin: '0 auto' }}>
      <SignupForm />
      <br />
      <span>
        I already have an account (<Link to="/login">go to login</Link>).
      </span>
    </div>
  )
}
```

SET `src/LoginPage.tsx`

```tsx
import { Link } from 'react-router-dom'
import { LoginForm } from 'wasp/client/auth'

export const LoginPage = () => {
  return (
    <div style={{ maxWidth: '400px', margin: '0 auto' }}>
      <LoginForm />
      <br />
      <span>
        I don't have an account yet (<Link to="/signup">go to signup</Link>).
      </span>
    </div>
  )
}
```

APPEND `main.wasp`

```wasp
// ...

page MainPage {
  authRequired: true,
  component: import { MainPage } from "@src/MainPage"
}
```

SET `src/MainPage.tsx`

```tsx
import { AuthUser } from 'wasp/auth'

export const MainPage = ({ user }: { user: AuthUser }) => {
  // Do something with the user
  // ...
}
```

APPEND `schema.prisma`

```prisma
// ...

model User {
  id    Int    @id @default(autoincrement())
  tasks Task[]
}

model Task {
  id          Int     @id @default(autoincrement())
  description String
  isDone      Boolean @default(false)
  user        User?   @relation(fields: [userId], references: [id])
  userId      Int?
}
```

RUN wasp db migrate-dev

SET `src/queries.ts`

```ts
import { Task } from 'wasp/entities'
import { HttpError } from 'wasp/server'
import { GetTasks } from 'wasp/server/operations'

export const getTasks: GetTasks<void, Task[]> = async (args, context) => {
  if (!context.user) {
    throw new HttpError(401)
  }
  return context.entities.Task.findMany({
    where: { user: { id: context.user.id } },
    orderBy: { id: 'asc' },
  })
}
```

SET `src/actions.ts`

```ts
import { Task } from 'wasp/entities'
import { HttpError } from 'wasp/server'
import { CreateTask, UpdateTask } from 'wasp/server/operations'

type CreateTaskPayload = Pick<Task, 'description'>

export const createTask: CreateTask<CreateTaskPayload, Task> = async (
  args,
  context
) => {
  if (!context.user) {
    throw new HttpError(401)
  }
  return context.entities.Task.create({
    data: {
      description: args.description,
      user: { connect: { id: context.user.id } },
    },
  })
}

type UpdateTaskPayload = Pick<Task, 'id' | 'isDone'>

export const updateTask: UpdateTask<
  UpdateTaskPayload,
  { count: number }
> = async ({ id, isDone }, context) => {
  if (!context.user) {
    throw new HttpError(401)
  }
  return context.entities.Task.updateMany({
    where: { id, user: { id: context.user.id } },
    data: { isDone },
  })
}
```

APPEND `src/MainPage.tsx`

```tsx
// ...
import { logout } from 'wasp/client/auth'
//...

const MainPage = () => {
  // ...
  return (
    <div>
      // ...
      <button onClick={logout}>Logout</button>
    </div>
  )
}
```