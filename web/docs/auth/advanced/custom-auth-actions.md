# Custom sign-up actions

If you need to deeply hook into the sign-up process, you can create your own sign-up action and customize the code to, for example, add extra validation, store more data, or otherwise call custom code at registration time.

:::danger

Custom sign-up actions are complex, and we don't recommend creating a custom sign-up action unless you have a good reason to do so.
They also require you to be careful, as any small mistake will compromise the security of your app.

Before using custom actions, check if our support for [custom auth UI](../overview.md#custom-auth-ui) and for [auth hooks](../auth-hooks.md) could fit well with you requirements.

:::

You are not able to use Wasp UI with custom sign-up actions, so you're expected to implemented your own UI and call the custom actions you create from it.

## Example code

### Email

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```wasp title="main.wasp"
// ...

action customSignup {
  fn: import { signup } from "@src/auth/signup.js",
}
```

```js title="src/auth/signup.js"
import { HttpError } from 'wasp/server'
import {
  createEmailVerificationLink,
  createProviderId,
  createUser,
  ensurePasswordIsPresent,
  ensureValidEmail,
  ensureValidPassword,
  findAuthIdentity,
  getProviderData,
  sanitizeAndSerializeProviderData,
  sendEmailVerificationEmail,
} from 'wasp/server/auth'

export const signup = async (args, _context) => {
  ensureValidEmail(args)
  ensurePasswordIsPresent(args)
  ensureValidPassword(args)

  try {
    const providerId = createProviderId('email', args.email)
    const existingAuthIdentity = await findAuthIdentity(providerId)

    if (existingAuthIdentity) {
      const providerData = getProviderData(existingAuthIdentity.providerData)
      // Your custom code here
    } else {
      // sanitizeAndSerializeProviderData will hash the user's password
      const providerData = await sanitizeAndSerializeProviderData({
        hashedPassword: args.password,
        isEmailVerified: false,
        emailVerificationSentAt: null,
        passwordResetSentAt: null,
      })
      await createUser(
        providerId,
        providerData,
        // Any additional data you want to store on the User entity
        {}
      )

      // Verification link links to a client route e.g. /email-verification
      const verificationLink = await createEmailVerificationLink(
        args.email,
        '/email-verification'
      )
      try {
        await sendEmailVerificationEmail(args.email, {
          from: {
            name: 'My App Postman',
            email: 'hello@itsme.com',
          },
          to: args.email,
          subject: 'Verify your email',
          text: `Click the link below to verify your email: ${verificationLink}`,
          html: `
                      <p>Click the link below to verify your email</p>
                      <a href="${verificationLink}">Verify email</a>
                  `,
        })
      } catch (e) {
        console.error('Failed to send email verification email:', e)
        throw new HttpError(500, 'Failed to send email verification email.')
      }
    }
  } catch (e) {
    return {
      success: false,
      message: e.message,
    }
  }

  // Your custom code after sign-up.
  // ...

  return {
    success: true,
    message: 'User created successfully',
  }
}
```

</TabItem>
<TabItem value="ts" label="TypeScript">

```wasp title="main.wasp"
// ...

action customSignup {
  fn: import { signup } from "@src/auth/signup.js",
}
```

```ts title="src/auth/signup.ts"
import { HttpError } from 'wasp/server'
import {
  createEmailVerificationLink,
  createProviderId,
  createUser,
  ensurePasswordIsPresent,
  ensureValidEmail,
  ensureValidPassword,
  findAuthIdentity,
  getProviderData,
  sanitizeAndSerializeProviderData,
  sendEmailVerificationEmail,
} from 'wasp/server/auth'
import type { CustomSignup } from 'wasp/server/operations'

type CustomSignupInput = {
  email: string
  password: string
}
type CustomSignupOutput = {
  success: boolean
  message: string
}

export const signup: CustomSignup<
  CustomSignupInput,
  CustomSignupOutput
> = async (args, _context) => {
  ensureValidEmail(args)
  ensurePasswordIsPresent(args)
  ensureValidPassword(args)

  try {
    const providerId = createProviderId('email', args.email)
    const existingAuthIdentity = await findAuthIdentity(providerId)

    if (existingAuthIdentity) {
      const providerData = getProviderData<'email'>(
        existingAuthIdentity.providerData
      )
      // Your custom code here
    } else {
      // sanitizeAndSerializeProviderData will hash the user's password
      const newUserProviderData =
        await sanitizeAndSerializeProviderData<'email'>({
          hashedPassword: args.password,
          isEmailVerified: false,
          emailVerificationSentAt: null,
          passwordResetSentAt: null,
        })
      await createUser(
        providerId,
        newUserProviderData,
        // Any additional data you want to store on the User entity
        {}
      )

      // Verification link links to a client route e.g. /email-verification
      const verificationLink = await createEmailVerificationLink(
        args.email,
        '/email-verification'
      )
      try {
        await sendEmailVerificationEmail(args.email, {
          from: {
            name: 'My App Postman',
            email: 'hello@itsme.com',
          },
          to: args.email,
          subject: 'Verify your email',
          text: `Click the link below to verify your email: ${verificationLink}`,
          html: `
                      <p>Click the link below to verify your email</p>
                      <a href="${verificationLink}">Verify email</a>
                  `,
        })
      } catch (e: unknown) {
        console.error('Failed to send email verification email:', e)
        throw new HttpError(500, 'Failed to send email verification email.')
      }
    }
  } catch (e: any) {
    return {
      success: false,
      message: e.message,
    }
  }

  // Your custom code after sign-up.
  // ...

  return {
    success: true,
    message: 'User created successfully',
  }
}
```

</TabItem>
</Tabs>

### Username and password


<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```wasp title="main.wasp"
// ...

action customSignup {
  fn: import { signup } from "@src/auth/signup.js",
}
```

```js title="src/auth/signup.js"
import {
  createProviderId,
  createUser,
  ensurePasswordIsPresent,
  ensureValidPassword,
  ensureValidUsername,
  sanitizeAndSerializeProviderData,
} from 'wasp/server/auth'

export const signup = async (args, _context) => {
  ensureValidUsername(args)
  ensurePasswordIsPresent(args)
  ensureValidPassword(args)

  try {
    const providerId = createProviderId('username', args.username)
    const providerData = await sanitizeAndSerializeProviderData({
      hashedPassword: args.password,
    })

    await createUser(
      providerId,
      providerData,
      // Any additional data you want to store on the User entity
      {}
    )
  } catch (e) {
    return {
      success: false,
      message: e.message,
    }
  }

  // Your custom code after sign-up.
  // ...

  return {
    success: true,
    message: 'User created successfully',
  }
}
```

</TabItem>
<TabItem value="ts" label="TypeScript">

```wasp title="main.wasp"
// ...

action customSignup {
  fn: import { signup } from "@src/auth/signup",
}
```

```ts title="src/auth/signup.ts"
import {
  createProviderId,
  createUser,
  ensurePasswordIsPresent,
  ensureValidPassword,
  ensureValidUsername,
  sanitizeAndSerializeProviderData,
} from 'wasp/server/auth'
import type { CustomSignup } from 'wasp/server/operations'

type CustomSignupInput = {
  username: string
  password: string
}
type CustomSignupOutput = {
  success: boolean
  message: string
}

export const signup: CustomSignup<
  CustomSignupInput,
  CustomSignupOutput
> = async (args, _context) => {
  ensureValidUsername(args)
  ensurePasswordIsPresent(args)
  ensureValidPassword(args)

  try {
    const providerId = createProviderId('username', args.username)
    const providerData = await sanitizeAndSerializeProviderData<'username'>({
      hashedPassword: args.password,
    })

    await createUser(
      providerId,
      providerData,
      // Any additional data you want to store on the User entity
      {}
    )
  } catch (e: any) {
    return {
      success: false,
      message: e.message,
    }
  }

  // Your custom code after sign-up.
  // ...

  return {
    success: true,
    message: 'User created successfully',
  }
}
```

</TabItem>
</Tabs>

## Reference

We suggest using the built-in field validators for your authentication flow. You can import them from `wasp/server/auth`. These are the same validators that Wasp uses internally for the default authentication flow.

#### Username

- `ensureValidUsername(args)`

  Checks if the username is valid and throws an error if it's not. Read more about the validation rules [here](../overview.md#default-validations).

#### Email

- `ensureValidEmail(args)`

  Checks if the email is valid and throws an error if it's not. Read more about the validation rules [here](../overview.md#default-validations).

#### Password

- `ensurePasswordIsPresent(args)`

  Checks if the password is present and throws an error if it's not.

- `ensureValidPassword(args)`

  Checks if the password is valid and throws an error if it's not. Read more about the validation rules [here](../overview.md#default-validations).

