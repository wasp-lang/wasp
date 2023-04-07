---
title: Auth UI
---

# Auth UI

![Auth UI](/img/authui/all_screens.gif)

## Usage

### The UI changes dynamically as you update the config

Based on your `main.wasp` file on the authentication providers you enabled, the Auth UI will show the corresponding buttons. 

For example, if you only enabled e-mail authentication:

```c title="main.wasp"
app MyApp {
  title: "My app",
  //...
  auth: {
    methods: {
      email: {},
    },
    // ...
  }
}
```

We'll get this:

![Auth UI](/img/authui/login.png)

And then we enable Google and Github:

```c title="main.wasp" {7-8}
app MyApp {
  title: "My app",
  //...
  auth: {
    methods: {
      email: {},
      google: {},
      github: {},
    },
    // ...
  }
}
```

The form will automatically update itself to look like this:

![Auth UI](/img/authui/multiple_providers.png)



## Available components

Let's take a look at the components that are available for you to use.

### Login form

Useful for <span style={{ background: '#fee', padding: '0.2rem 0.5rem', borderRadius: '0.375rem' }}>username & password</span> and <span style={{ background: '#eef', padding: '0.2rem 0.5rem', borderRadius: '0.375rem' }}>email</span> authentication.

![Login form](/img/authui/login.png)

You can use the `LoginForm` component to build your own login form.

```tsx title="client/LoginPage.tsx"
import { LoginForm } from '@wasp/auth/forms/Login'

// Use it like this
<LoginForm />
```

### Signup form

Useful for <span style={{ background: '#fee', padding: '0.2rem 0.5rem', borderRadius: '0.375rem' }}>username & password</span> and <span style={{ background: '#eef', padding: '0.2rem 0.5rem', borderRadius: '0.375rem' }}>email</span> authentication.

![Signup form](/img/authui/signup.png)

You can use the `SignupForm` component to build your own signup form.

```tsx title="client/SignupPage.tsx"
import { SignupForm } from '@wasp/auth/forms/Signup'

// Use it like this
<SignupForm />
```

### Forgot password form

Useful for <span style={{ background: '#eef', padding: '0.2rem 0.5rem', borderRadius: '0.375rem' }}>email</span> authentication.

![Forgot password form](/img/authui/forgot_password.png)

You can use the `ForgotPasswordForm` component to build your own forgot password form.

```tsx title="client/ForgotPasswordPage.tsx"
import { ForgotPasswordForm } from '@wasp/auth/forms/ForgotPassword'

// Use it like this
<ForgotPasswordForm />
```

### Reset password form

Useful for <span style={{ background: '#eef', padding: '0.2rem 0.5rem', borderRadius: '0.375rem' }}>email</span> authentication.

![Reset password form](/img/authui/reset_password.png)

You can use the `ResetPasswordForm` component to build your own reset password form.

```tsx title="client/ResetPasswordPage.tsx"
import { ResetPasswordForm } from '@wasp/auth/forms/ResetPassword'

// Use it like this
<ResetPasswordForm />
```

### Verify email form

Useful for <span style={{ background: '#eef', padding: '0.2rem 0.5rem', borderRadius: '0.375rem' }}>email</span> authentication.

![Verify email form](/img/authui/email_verification.png)

You can use the `VerifyEmailForm` component to build your own verify email form.

```tsx title="client/VerifyEmailPage.tsx"
import { VerifyEmailForm } from '@wasp/auth/forms/VerifyEmail'

// Use it like this
<VerifyEmailForm />
```

## Customization

You customize all of the available forms by passing props to them. 

Props you can pass to all of the forms:
- `appearance` - appearance of the form, see below (optional)
- `logo` - path to your logo (optional)
- `socialLayout` - layout of the social buttons, can be `vertical` or `horizontal` (optional)

### Theme colors override

We used [Stitches](https://stitches.dev/) to style the Auth UI. You can customize the styles by overriding the default theme tokens. 

```tsx title="client/LoginPage.tsx"
import { LoginForm } from '@wasp/auth/forms/Login'

// Define your appearance
const appearance = {
  colors: {
    brand: '#5969b8', // blue
    brandAccent: '#de5998', // pink
    submitButtonText: 'white',
  },
}

// Use it like this
<LoginForm appearance={appearance} />
```

See the list of all available tokens [here](https://github.com/wasp-lang/wasp/blob/main/waspc/data/Generator/templates/react-app/src/stitches.config.js).

### Adding your logo

You can add your logo to the Auth UI by passing the `logo` prop to any of the components.

```tsx title="client/LoginPage.tsx"
import { LoginForm } from '@wasp/auth/forms/Login'

// Use it like this
<LoginForm logo="/img/logo.png" />
```

### Social buttons layout

You can change the layout of the social buttons by passing the `socialLayout` prop to the any of the components.

If we pass in `vertical`:

```tsx title="client/LoginPage.tsx"
import { LoginForm } from '@wasp/auth/forms/Login'

// Use it like this
<LoginForm
  socialLayout="vertical"
/>
```

We get this:

![Vertical social buttons](/img/authui/vertical_social_buttons.png)

### Example of a custom login form

If we provide the logo and custom colors:
```tsx title="client/LoginPage.tsx"
import { LoginForm } from '@wasp/auth/forms/Login'

import { appearance } from './appearance'
import todoLogo from '../../todoLogo.png'

// Use it like this
<LoginForm
  appearance={appearance}
  logo={todoLogo}
/>
```

We get this:

<div style={{ textAlign: 'center' }}>
  <img src="/img/authui/custom_login.gif" alt="Custom login form" />
</div>
