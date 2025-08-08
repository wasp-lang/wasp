# Custom sign-up actions

If you need to deeply hook into the sign-up process, you can create your own sign-up action and customize the code to, for example, add extra validation, store more data, or otherwise call custom code at registration time.

:::danger

Custom sign-up actions are complex, and we don't recommend creating a custom sign-up action unless you have a good reason to do so.
They also require you to be careful, as any small mistake will compromise the security of your app.

Before using custom actions, check if our support for [custom auth UI](../overview.md#custom-auth-ui) and for [auth hooks](../auth-hooks.md) could fit well with you requirements.

:::

You are not able to use Wasp UI with custom sign-up actions, so you're expected to implemented your own UI and call the custom actions you create from it.

## Example code

Below you will find a starting point for creating your own actions. The given implementation is similar to what Wasp does under the hood, and it is up to you to customize it.

### Email

```wasp title="main.wasp"
app myApp {
  // ...
  auth: {
    // ...
    onBeforeSignup: import { onBeforeSignup } from "@src/auth/hooks",
  },
}

// ...

action customSignup {
  fn: import { signup } from "@src/auth/signup",
}
```

```ts title="src/auth/hooks.ts" auto-js
import { HttpError } from 'wasp/server'

// This disables Wasp's default sign-up action
export const onBeforeSignup = async () => {
  throw new HttpError(403, 'This sign-up method is disabled')
}
```

```ts title="src/auth/signup.ts" auto-js
import type { CustomSignup } from "wasp/server/operations";
import { HttpError } from "wasp/server";
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
} from "wasp/server/auth";

type CustomSignupInput = {
  email: string;
  password: string;
};

type CustomSignupOutput = {
  success: boolean;
  message: string;
};

export const signup: CustomSignup<
  CustomSignupInput,
  CustomSignupOutput
> = async (args, _context) => {
  ensureValidEmail(args);
  ensurePasswordIsPresent(args);
  ensureValidPassword(args);

  try {
    const providerId = createProviderId("email", args.email);
    const existingAuthIdentity = await findAuthIdentity(providerId);

    let providerData;

    if (existingAuthIdentity) {
      // User already exists, handle accordingly

      // For example, throw an error or return a message
      throw new HttpError(400, "Email already exists.");

      // Or, another example, you can check if the user is already
      // verified and re-send the verification email if not
      providerData = getProviderData<"email">(
        existingAuthIdentity.providerData,
      );
      if (providerData.isEmailVerified)
        throw new HttpError(400, "Email already verified.");
    }

    if (!providerData) {
      providerData = await sanitizeAndSerializeProviderData<"email">({
        // The provider will hash the password for us, so we don't need to do it here.
        hashedPassword: args.password,
        isEmailVerified: false,
        emailVerificationSentAt: null,
        passwordResetSentAt: null,
      });
      await createUser(
        providerId,
        providerData,
        // Any additional data you want to store on the User entity
        {},
      );
    }

    // Verification link links to a client route e.g. /email-verification
    const verificationLink = await createEmailVerificationLink(
      args.email,
      "/email-verification",
    );
    try {
      await sendEmailVerificationEmail(args.email, {
        from: {
          name: "My App Postman",
          email: "hello@itsme.com",
        },
        to: args.email,
        subject: "Verify your email",
        text: `Click the link below to verify your email: ${verificationLink}`,
        html: `
          <p>Click the link below to verify your email</p>
          <a href="${verificationLink}">Verify email</a>
        `,
      });
    } catch (e: unknown) {
      console.error("Failed to send email verification email:", e);
      throw new HttpError(500, "Failed to send email verification email.");
    }
  } catch (e: any) {
    return {
      success: false,
      message: e.message,
    };
  }

  // Your custom code after sign-up.
  // ...

  return {
    success: true,
    message: "User created successfully",
  };
};
```

### Username and password

```wasp title="main.wasp"
app myApp {
  // ...
  auth: {
    // ...
    onBeforeSignup: import { onBeforeSignup } from "@src/auth/hooks",
  },
}

// ...

action customSignup {
  fn: import { signup } from "@src/auth/signup",
}
```

```ts title="src/auth/hooks.ts" auto-js
import { HttpError } from 'wasp/server'

// This disables Wasp's default sign-up action
export const onBeforeSignup = async () => {
  throw new HttpError(403, 'This sign-up method is disabled')
}
```

```ts title="src/auth/signup.ts" auto-js
import type { CustomSignup } from "wasp/server/operations";
import {
  createProviderId,
  createUser,
  ensurePasswordIsPresent,
  ensureValidPassword,
  ensureValidUsername,
  sanitizeAndSerializeProviderData,
} from "wasp/server/auth";

type CustomSignupInput = {
  username: string;
  password: string;
};

type CustomSignupOutput = {
  success: boolean;
  message: string;
};

export const signup: CustomSignup<
  CustomSignupInput,
  CustomSignupOutput
> = async (args, _context) => {
  ensureValidUsername(args);
  ensurePasswordIsPresent(args);
  ensureValidPassword(args);

  try {
    const providerId = createProviderId("username", args.username);
    const providerData = await sanitizeAndSerializeProviderData<"username">({
      // The provider will hash the password for us, so we don't need to do it here.
      hashedPassword: args.password,
    });

    await createUser(providerId, providerData, {});
  } catch (e: any) {
    console.error("Error creating user:", e);
    return {
      success: false,
      message: e.message,
    };
  }

  return {
    success: true,
    message: "User created successfully",
  };
};
```

## Validators API Reference

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
