---
title: Feature Announcement - TypeScript Support
authors: [sodic]
image: /img/wasp-ts-banner.png
tags: [webdev, wasp, feature, typescript, javascript]
---

import Link from '@docusaurus/Link';
import useBaseUrl from '@docusaurus/useBaseUrl';
import InBlogCta from './components/InBlogCta';
import WaspIntro from './_wasp-intro.md';
import ImgWithCaption from './components/ImgWithCaption'

<ImgWithCaption
    alt="Wasp TS support"
    source="img/wasp-ts-banner.png"
/>

<!--truncate-->

<InBlogCta />

## Prologue
TypeScript doesn't need much introduction at this point, so we'll keep it short!
Wasp finally allows you to write your code in TypeScript (i.e., the most popular web technology after JavaScript) on both the front-end and the back-end.

You can now define and use types in any part of your code, enjoying all benefits of the static type checker. At the time of writing, not all parts of Wasp are typed as well as they could be, but we're working on it!
Exposing all Wasp functionalities through informative typed interfaces is one of our top priorities.

Without further ado, let's see how we can use TypeScript with Wasp.

## Setting up a TypeScript project in Wasp
Let's start by creating a fresh Wasp project:

```bash
wasp new ts-project
```
This will generate a project skeleton in the folder `ts-project`. The project structure is different than before, and there are now several additional generated files that help with IDE and TypeScript support. So let's explain it:
```
.
├── .gitignore
├── main.wasp             # Your wasp code goes here.
├── src
│   ├── client            # Your client code (JS/CSS/HTML) goes here.
│   │   ├── Main.css
│   │   ├── MainPage.jsx
│   │   ├── react-app-env.d.ts
│   │   ├── tsconfig.json
│   │   └── waspLogo.png
│   ├── server            # Your server code (Node JS) goes here.
│   │   └── tsconfig.json
│   ├── shared            # Your shared (runtime independent) code goes here.
│   │   └── tsconfig.json
│   └── .waspignore
└── .wasproot
```


At this point, we can chose one of three options:
1. We write our code exclusively in JavaScript.
2. We write our code exclusively in TypeScript.
3. We write some parts of our code in JavaScript, and other parts in TypeScript.

Since the third option is a superset of the first two, that's what Wasp currently supports. In other words, regardless of whether you want your entire codebase in one of these languages or you want to mix it up, there's no extra configuration necessary!
Simply use the appropriate extension (`.ts` and `.tsx` for TypeScript; `.js` and `.jsx` for JavaScript), and your IDE and Wasp will know what to do.

To demonstrate this, let's start Wasp and change `MainPage.jsx` to `MainPage.tsx`:
```
wasp start
mv src/client/MainPage.jsx src/client/MainPage.tsx
```
That's it! Wasp will notice the change and recompile, and your app will continue to work. The only difference is that you can now write TypeScript in `MainPage.tsx` and get helpful information from your IDE and the static type checker. Try removing an import and see what happens.

The same applies to any file you may want to include in your project. Specify the language you wish to use via the extension, and Wasp will do the rest!

:::caution
Even if you use TypeScript and have a _server_ file called `someFile.ts`, you must still import it as if it had the `.js` extension (i.e., `import foo from 'someFile.js'`). Wasp internally uses `esnext` module resolution, which always requires specifying the extension as `.js` (i.e., the extension used in the emitted JS file). This applies to all `@server` imports (and files on the server in general).

Read more about ES modules in TypeScript [here](https://www.typescriptlang.org/docs/handbook/esm-node.html). If you're interested in the discussion and the reasoning behind this, read about it [in this GitHub issue](https://github.com/microsoft/TypeScript/issues/33588).

This does not apply to front-end files. Thanks to Webpack, you don't need to write extensions when working with client-side imports.

:::

## Moving existing projects to the new structure (and optionally TypeScript)
If you wish to move an existing project to the new structure, the easiest approach comes down to creating a new project and moving all the files from your old project into appropriate locations. After doing this, you can choose which files you'd like to implement in TypeScript, change the extension and go for it.


To avoid digging too deep, this is all we'll say about migrating. For a more detailed migration guide, check [our changelog](https://github.com/wasp-lang/wasp/releases/tag/v0.7.0). It explains everything step-by-step.

## TypeScript in action
Finally, let's demonstrate how TypeScript helps us by using it in a small Todo app. The part of our code in charge of rendering tasks looks something like this:
```jsx

function MainPage() {
  const { data: tasks } = useQuery(getTasks)

  return (
    <div>
      <h1>Todos</h1>
      <TaskList tasks={tasks} />
    </div>
  )
}

function TaskList({ tasks }) {
  if (!tasks.len) {
    return <div>No tasks</div>
  }

  return (
    <div>
      {tasks.map((task, idx) => <Task {...task} key={idx}/>)}
    </div>
  )
}



function Task({ id, isdone, description }) {
  return (
    <div>
      <label>
        <input
          type='checkbox'
          id={id}
          checked={isdone}
          onChange={
            (event) => updateTask({ id, isDone: event.target.checked })
          }
        />
        <span>{description}</span>
      </label>
    </div>
  )
}
```
Try to see if you can find any bugs. When you're confident you've got all of them, continue reading.

Let's see what happens when we bring TypeScript into the picture. Remember, we only need to change the extension to `tsx`. After we do this, The IDE will warn us about missing type definitions, so let's fill these in. While we're at it, we can also tell `useQuery` what types it's working with by specifying its type arguments.

Here's how our code looks after these changes:
```tsx
// highlight-start
type Task = {
  id: string
  description: string
  isDone: boolean
}
// highlight-end

function MainPage() {
  // highlight-next-line
  const { data: tasks } = useQuery<Task, Task[]>(getTasks)

  return (
    <div>
      <h1>Todos</h1>
      <TaskList tasks={tasks} />
    </div>
  )
}

// highlight-next-line
function TaskList({ tasks }: { tasks: Task[] }) {
  if (!tasks.len) {
    return <div>No tasks</div>
  }

  return (
    <div>
      {tasks.map((task, idx) => <Task {...task} key={idx}/>)}
    </div>
  )
}



// highlight-next-line
function Task({ id, isdone, description }: Task) {
  return (
    <div>
      <label>
        <input
          type='checkbox'
          id={id}
          checked={isdone}
          onChange={
            (event) => updateTask({ id, isDone: event.target.checked })
          }
        />
        <span>{description}</span>
      </label>
    </div>
  )
}
```
As soon as we change our code, TypeScript detects three errors:

<ImgWithCaption
    alt="TypeScript erros"
    source="img/typescript-errors.png"
    caption="The errors are pretty simple (almost as if we've made them up for this example :)"
/>

1. The first error warns us that `tasks` might be `undefined` (e.g., on the first render), which `TaskList` does not expect
2. The second error tells us that the property `len` does not exist on the array `tasks`. In other words, we misspelled `length`.
3. Finally, the third error tells us that the type `Task` does not contain the field `isdone`. This is also a typo. The field's name should be `isDone`.

Thanks to TypeScript, we can quickly fix all three errors, saving us a lot of time we'd probably lose by hunting them down manually or, even worse, during runtime.
```tsx

type Task = {
  id: string
  description: string
  isDone: boolean
}
function MainPage() {
  const { data: tasks } = useQuery<Task, Task[]>(getTasks)

  return (
    <div>
      <h1>Todos</h1>
      // highlight-next-line
      {tasks && <TaskList tasks={tasks} />}
    </div>
  )
}

function TaskList({ tasks }: { tasks: Task[] }) {
  // highlight-next-line
  if (!tasks.length) {
    return <div>No tasks</div>
  }

  return (
    <div>
      {tasks.map((task, idx) => <Task {...task} key={idx} />)}
    </div>
  )
}



// highlight-next-line
function Task({ id, isDone, description }: Task) {
  return (
    <div>
      <label>
        <input
          type='checkbox'
          id={id}
          // highlight-next-line
          checked={isDone}
          onChange={
            (event) => updateTask({ id, isDone: event.target.checked })
          }
        />
        <span>{description}</span>
      </label>
    </div>
  )
}
```

And that's it! This is the joy of TypeScript. We've easily fixed all reported errors, and our code should now work correctly (well, at least less incorrectly).

## Future work
You might have noticed that, if we want to use the `Task` type, we have to write most of its type definition twice - once when defining the `Task` entity in the `.wasp` file and then again in our code. While we can define the type in `src/shared` to avoid writing (almost) the same code on both the server and the client, we'll still have duplication between the code in `src/shared` and our `.wasp` file.

The good news is that we know about this, also find it annoying, and are working to fix it as soon as possible! In the near future, Wasp will generate types from entities and allow you to access them using `@wasp` imports. Other improvements exist, too. For example, Wasp could read your query declarations and provide you with the correct type for the `context` object in their definitions. Another possible improvement is automatically typing queries on the front-end, and then relying on type inference to correctly type `useQuery` (instead of users specifying its type arguments explicitly).

In short, there's a long and exciting path ahead of us, full of interesting possibilities. So stick with Wasp and see how far we can make it!

