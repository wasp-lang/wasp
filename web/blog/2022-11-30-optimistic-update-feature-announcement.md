---
title: Feature Release Announcement - Wasp Optimistic Updates
authors: [sodic]
image: /img/opt-updates-banner.png
tags: [webdev, wasp, feature, optimistic, updates]
---

import Link from '@docusaurus/Link';
import useBaseUrl from '@docusaurus/useBaseUrl';
import InBlogCta from './components/InBlogCta';
import WaspIntro from './_wasp-intro.md';
import ImgWithCaption from './components/ImgWithCaption'

We’re excited to announce that Wasp actions now feature native support for optimistic updates!
Continue reading to to find out what optimistic updates are and how Wasp implements them.

<ImgWithCaption
    alt="Wasp TS support"
    source="img/opt-updates-banner.png"
/>

<!--truncate-->

## What are Optimistic Updates Anyway?

Think about an interactive web app you use daily. It could be almost anything (e.g., Reddit, Youtube, Facebook). It almost certainly features UI elements you can interact with without refreshing the page, such as upvotes on Reddit or likes on Youtube.

All these small actions play out in the same manner. Let's look at Reddit upvotes as an example:

1. You click on the upvote button
2. Your browser sends a request to the server to save the upvote
3. The server saves your upvote to the database and sends a successful response to your browser
4. Your browser receives the successful response and reflects the change in the UI (i.e., you see your upvote)

The client *waits* for the server's confirmation *before* updating the UI because actions can sometimes fail. Well, at least that was the original idea.

These days, many popular websites update their UIs *without waiting* for servers' responses. Most of the time, everything goes as expected: you click on an upvote, and the server returns a successful response a couple of seconds later (depending on how fast your connection is). Since programmers want their users to have a snappier experience, instead of waiting for a confirmation, they update the UI immediately (as if the action were successful) and then roll back if the server doesn't return a successful response (which rarely happens). This pattern of optimistically updating the UI before receiving the confirmation of success is called, you guessed it, an **Optimistic Update**.

Most popular modern websites use optimistic updates to some degree. As mentioned, Reddit uses them for upvotes and downvotes, Youtube uses them for likes, and Trello uses them when moving cards between lists.

Optimistic updates are a significant UX improvement, but since they introduce additional state (which can get out of sync with the server), they can be tricky to get right. Then there's also the issue of writing additional code for managing the cache and rolling back the changes if the request ends up failing. Luckily, we're here to help!

Wasp recently added native support for optimistic updates, and the rest of this post demonstrates how to quickly set it up in your Wasp application.

## A Wasp Todo App Without Optimistic Updates

To honor the tradition of demonstrating UIs using Todo apps, We'll show you how to improve the UX of toggling an item's status when working with a slow connection.
Before looking at our todo app in action, let's see how we've implemented it in Wasp.

These are the relevant declarations in our `.wasp` file:
```javascript title=main.wasp
entity Task {=psl
    id          Int     @id @default(autoincrement())
    description String
    isDone      Boolean @default(false)
psl=}

// A query for fetching all tasks.
query getTasks {
  fn: import { getTasks } from "@server/queries.js",
  entities: [Task]
}


// An action for updating the task's status.
action updateTask {
  fn: import { updateTask } from "@server/actions.js",
  entities: [Task]
}
```
This is the query we use to fetch the tasks (together with their statuses):
```javascript title=queries.js
export const getTasks = async (args, context) => {
  return context.entities.Task.findMany()
}
```
Here's the action we use to update a task’s status:
```javascript title=actions.js
export const updateTask = async ({ id, isDone }, context) => {
  return context.entities.Task.updateMany({
    where: { id },
    data: { isDone }
  })
}
```
Finally, this is how our client uses this action to update a task:
```jsx title=MainPage.js
import updateTask from '@wasp/queries'

// ...

function Task({ id, isDone, description }) {
  return (
    <div className="task">
      <label className="description">
        <input
          type='checkbox' id={id}
          checked={isDone}
          onChange={
            (e) => updateTask({ id, isDone: e.target.checked })
          }
        /><span>{description}</span></label>
    </div>
  )
}
```
Let's first see how updating a task looks when everything works as expected (i.e., we're on a fast connection):

<ImgWithCaption
    alt="Normal todo list"
    source="img/optimistic-update-feature-announcement-normal.gif"
/>

So far, so good! But what happens when our connection is not as fast?

<ImgWithCaption
    alt="Todo list with lag"
    source="img/optimistic-update-feature-announcement-lag.gif"
/>

Hmm, this isn't quite as smooth as we'd like it to be.
The user has to wait for several seconds before seeing their their changes reflected by the UI.

How can we improve it? Well, of course, we can optimistically update the checkbox!

## Performing a Wasp Action Optimistically
To perform the `updateTask` action optimistically, all we need to do is decorate the calling code on the client:
```jsx {6-16,25} title=MainPage.js 
import updateTask from '@wasp/queries'

// ...

function Task({ id, isDone, description }) {
  const updateTaskOptimistically = useAction(updateTask, {
    optimisticUpdates: [{
      // Addressing the query we want to update.
      getQuerySpecifier: () => [getTasks],
      // Telling Wasp how to update the addressed query using the new payload
      // and the previously cached data.
      updateQuery: ({ id, isDone }, oldTasks) => oldTasks.map(
        task => task.id === id ? { ...task, isDone } : task
      )
    }]
  })

  return (
    <div className="task">
      <label className="description">
        <input
          type='checkbox' id={id}
          checked={isDone}
          onChange={
            (e) => updateTaskOptimistically({ id, isDone: e.target.checked })
          }
        /><span>{description}</span></label>
    </div>
  )
}
```
Those are all the changes we need, the rest of the code (i.e., `main.wasp`, `queries.js` and `actions.js`) remains the same. We won't describe the API in detail, but if you're curious, everything is covered by [our official docs](/docs/language/features#the-useaction-hook).

Finally, let's see how this version of the app looks in action:

<ImgWithCaption
    alt="Optimistically updated todo list"
    source="img/optimistic-update-feature-announcement-fixed.gif"
/>


Our app no longer waits for the server before rendering the changes. Instead, it updates the cache optimistically, continues waiting for the response, and rolls back the changes if the action fails (Wasp internally handles all of this). As previously mentioned, simple changes such as this one rarely fail. Therefore, most of the time, the user enjoys their snappier experience without ever knowing anything special is happening in the background.

## What Makes Optimistic Updates Difficult 
There's an old software engineering joke you're probably familiar with:
> There are only two hard things in Computer Science: cache invalidation and naming things.

Optimistically updating a query involves plenty of meddling with the client-side cache, which is bound to come with a few gotchas. Examples include the answers to questions such as:

- What happens when an optimistically updated action fails?
- What happens when the user uses the optimistically updated data in a new action?
- What happens when the user performs a different action that affects the same cached data as the optimistically updated one?
- etc.

Notice how Wasp users don't need to know about any of these issues when using our optimistic updates API. They only need to tell Wasp which query they wish to update and how, and Wasp takes care of the rest.

Wasp internally uses [React Query](https://tanstack.com/query/v4/docs/adapters/react-query), an excellent asynchronous state management library we'll gladly recommend to anyone. While React Query does solve some of these problems and helps with some of the rest, we still had to implement quite a complex mechanism to fully cover all edge cases.

Describing this mechanism, although technically interesting, is beyond the scope of a feature announcement. But stay tuned because in a future blog post, we'll be taking a deep dive into the infrastructure Wasp uses to ensure optimistic updates are performed correctly and consistently.
