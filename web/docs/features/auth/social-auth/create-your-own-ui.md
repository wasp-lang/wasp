---
title: Create your own UI for social auth in Wasp
sidebar_label: Create your own UI
description: Build your own social login page in Wasp with the sign-in buttons and URLs that Wasp provides for each supported provider, such as Google and GitHub.
---

import { CardLink } from '@site/src/components/CardLink'

# Create your own UI

[Auth UI](../ui.md) is a common name for all high-level auth forms that come with Wasp.

These include fully functional auto-generated login and signup forms with working social login buttons.
If you're looking for the fastest way to get your auth up and running, that's where you should look.

The UI helpers described below are lower-level and are useful for creating your custom login links.

Wasp provides sign-in buttons and URLs for each of the supported social login providers.

```tsx title="src/LoginPage.tsx" auto-js
import {
  GoogleSignInButton,
  googleSignInUrl,
  GitHubSignInButton,
  githubSignInUrl,
} from "wasp/client/auth";

export const LoginPage = () => {
  return (
    <>
      <GoogleSignInButton />
      <GitHubSignInButton />
      {/* or */}
      <a href={googleSignInUrl}>Sign in with Google</a>
      <a href={githubSignInUrl}>Sign in with GitHub</a>
    </>
  );
};
```

## API Reference

<CardLink
  to="../../../api/wasp/client/auth"
  kind="api"
  title="wasp/client/auth"
  description="The sign-in buttons and URLs of each social auth provider, like GoogleSignInButton and googleSignInUrl."
/>
