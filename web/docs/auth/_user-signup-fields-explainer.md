`userSignupFields` defines all the extra fields that need to be set on the `User` during the sign-up process. For example, if you have `address` and `phone` fields on your `User` entity, you can set them by defining the `userSignupFields` like this:

<Tabs groupId="js-ts">
  <TabItem value="js" label="JavaScript">
    ```ts title="src/auth.js" 
    import { defineUserSignupFields } from 'wasp/server/auth'

    export const userSignupFields = defineUserSignupFields({
      address: (data) => {
        if (typeof data.address !== 'string') {
          throw new Error('Address is required.')
        }
        if (data.address.length < 10) {
          throw new Error('Address must be at least 10 characters long.')
        }
        return data.address
      },
    })
    ```
  </TabItem>

  <TabItem value="ts" label="TypeScript">
    ```ts title="src/auth.ts" ref="waspc/examples/todoApp/src/auth/signup.ts:L1-14"
    import { defineUserSignupFields } from 'wasp/server/auth'

    export const userSignupFields = defineUserSignupFields({
      address: (data) => {
        if (typeof data.address !== 'string') {
          throw new Error('Address is required.')
        }
        if (data.address.length < 10) {
          throw new Error('Address must be at least 10 characters long.')
        }
        return data.address
      },
    })
    ```
  </TabItem>
</Tabs>
