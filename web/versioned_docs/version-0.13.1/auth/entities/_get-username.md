The `getUsername` helper returns the user's username or `null` if the user doesn't have a username auth identity.

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```jsx title="src/MainPage.jsx"
import { getUsername } from 'wasp/auth'

const MainPage = ({ user }) => {
  const username = getUsername(user)
  // ...
}
```

```js title=src/tasks.js
import { getUsername } from 'wasp/auth'

export const createTask = async (args, context) => {
  const username = getUsername(context.user)
  // ...
}
```


</TabItem>
<TabItem value="ts" label="TypeScript">

```tsx title="src/MainPage.tsx"
import { getUsername, AuthUser } from 'wasp/auth'

const MainPage = ({ user }: { user: AuthUser }) => {
  const username = getUsername(user)
  // ...
}
```

```ts title=src/tasks.ts
import { getUsername } from 'wasp/auth'

export const createTask: CreateTask<...>  = async (args, context) => {
  const username = getUsername(context.user)
  // ...
}
```

</TabItem>
</Tabs>