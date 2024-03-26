The `getEmail` helper returns the user's email or `null` if the user doesn't have an email auth identity.

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```jsx title="src/MainPage.jsx"
import { getEmail } from 'wasp/auth'

const MainPage = ({ user }) => {
  const email = getEmail(user)
  // ...
}
```

```js title=src/tasks.js
import { getEmail } from 'wasp/auth'

export const createTask = async (args, context) => {
  const email = getEmail(context.user)
  // ...
}
```


</TabItem>
<TabItem value="ts" label="TypeScript">

```tsx title="src/MainPage.tsx"
import { getEmail, AuthUser } from 'wasp/auth'

const MainPage = ({ user }: { user: AuthUser }) => {
  const email = getEmail(user)
  // ...
}
```

```ts title=src/tasks.ts
import { getEmail } from 'wasp/auth'

export const createTask: CreateTask<...>  = async (args, context) => {
  const email = getEmail(context.user)
  // ...
}
```

</TabItem>
</Tabs>
