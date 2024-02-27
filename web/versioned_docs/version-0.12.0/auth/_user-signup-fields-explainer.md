`userSignupFields` defines all the extra fields that need to be set on the `User` during the sign-up process. For example, if you have `address` and `phone` fields on your `User` entity, you can set them by defining the `userSignupFields` like this:

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```ts title="src/auth.js"
import { defineUserSignupFields } from 'wasp/server/auth'

export const userSignupFields = defineUserSignupFields({
  address: (data) => {
    if (!data.address) {
      throw new Error('Address is required')
    }
    return data.address
  }
  phone: (data) => data.phone,
})
```

</TabItem>
<TabItem value="ts" label="TypeScript">

```ts title="src/auth.ts"
import { defineUserSignupFields } from 'wasp/server/auth'

export const userSignupFields = defineUserSignupFields({
  address: (data) => {
    if (!data.address) {
      throw new Error('Address is required')
    }
    return data.address
  }
  phone: (data) => data.phone,
})
```

</TabItem>
</Tabs>
