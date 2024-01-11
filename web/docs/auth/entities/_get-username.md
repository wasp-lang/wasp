The `getUsername` helper returns the user's username or `null` if the user doesn't have a username auth identity.

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```jsx title="src/client/MainPage.jsx"
import { getUsername } from '@wasp/auth/user'

const MainPage = ({ user }) => {
  const username = getUsername(user)
  // ...
}
```

```js title=src/server/tasks.js
import { getUsername } from '@wasp/auth/user.js'

export const createTask = async (args, context) => {
  const username = getUsername(context.user)
  // ...
}
```


</TabItem>
<TabItem value="ts" label="TypeScript">

```tsx title="src/client/MainPage.tsx"
import { getUsername } from '@wasp/auth/user'
import { User } from '@wasp/auth/types'

const MainPage = ({ user }: { user: User }) => {
  const username = getUsername(user)
  // ...
}
```

```ts title=src/server/tasks.ts
import { getUsername } from '@wasp/auth/user.js'

export const createTask: CreateTask<...>  = async (args, context) => {
  const username = getUsername(context.user)
  // ...
}
```

</TabItem>
</Tabs>