The `getEmail` helper returns the user's email or `null` if the user doesn't have an email auth identity.

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```jsx title="src/client/MainPage.jsx"
import { getEmail } from '@wasp/auth/user'

const MainPage = ({ user }) => {
  const email = getEmail(user)
  // ...
}
```

```js title=src/server/tasks.js
import { getEmail } from '@wasp/auth/user.js'

export const createTask = async (args, context) => {
  const email = getEmail(context.user)
  // ...
}
```


</TabItem>
<TabItem value="ts" label="TypeScript">

```tsx title="src/client/MainPage.tsx"
import { getEmail } from '@wasp/auth/user'
import { User } from '@wasp/auth/types'

const MainPage = ({ user }: { user: User }) => {
  const email = getEmail(user)
  // ...
}
```

```ts title=src/server/tasks.ts
import { getEmail } from '@wasp/auth/user.js'

export const createTask: CreateTask<...>  = async (args, context) => {
  const email = getEmail(context.user)
  // ...
}
```

</TabItem>
</Tabs>
