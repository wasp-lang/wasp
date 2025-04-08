---
title: Create your own UI
---

[Auth UI](../ui.md) is a common name for all high-level auth forms that come with Wasp.

These include fully functional auto-generated login and signup forms with working social login buttons.
If you're looking for the fastest way to get your auth up and running, that's where you should look.

The UI helpers described below are lower-level and are useful for creating your custom login links.

Wasp provides sign-in buttons and URLs for each of the supported social login providers.

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```jsx title=src/LoginPage.jsx
import {
  GoogleSignInButton,
  googleSignInUrl,
  GitHubSignInButton,
  gitHubSignInUrl,
} from 'wasp/client/auth'

export const LoginPage = () => {
  return (
    <>
      <GoogleSignInButton />
      <GitHubSignInButton />
      {/* or */}
      <a href={googleSignInUrl}>Sign in with Google</a>
      <a href={gitHubSignInUrl}>Sign in with GitHub</a>
    </>
  )
}
```

</TabItem>
<TabItem value="ts" label="TypeScript">

```tsx title=src/LoginPage.tsx
import {
  GoogleSignInButton,
  googleSignInUrl,
  GitHubSignInButton,
  gitHubSignInUrl,
} from 'wasp/client/auth'

export const LoginPage = () => {
  return (
    <>
      <GoogleSignInButton />
      <GitHubSignInButton />
      {/* or */}
      <a href={googleSignInUrl}>Sign in with Google</a>
      <a href={gitHubSignInUrl}>Sign in with GitHub</a>
    </>
  )
}
```

</TabItem>
</Tabs>
