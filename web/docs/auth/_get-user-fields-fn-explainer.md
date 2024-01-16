`getUserFieldsFn` defines all the extra fields that need to be set on the `User` during the sign-up process. For example, if you have `address` and `phone` fields on your `User` entity, you can set them in the `getUserFieldsFn` function like this:

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```ts title="server/auth.js"
import { defineUserFields } from '@wasp/auth/index.js'

export const getUserFields = () => defineUserFields({
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

```ts title="server/auth.ts"
import { defineUserFields } from '@wasp/auth/index.js'

export const getUserFields = () => defineUserFields({
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

Read more about the `getUserFieldsFn` function [here](../auth/overview#1-defining-extra-fields).