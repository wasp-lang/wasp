---
title: Create your own UI
title-llm: Create your own UI for Social Auth
---

[Auth UI](../ui.md) is a common name for the drop-in auth form examples Wasp provides.

These include fully functional login and signup examples with working social login buttons. If you're looking for the fastest way to get your auth up and running, that's where you should look.

The UI helpers described below are lower-level and are useful for creating your custom login links.

Wasp provides sign-in URLs and generated provider metadata for each enabled social login provider.

```tsx title="src/LoginPage.tsx" auto-js
import {
  enabledOAuthProviders,
  providerIconById,
} from "wasp/client/auth";

export const LoginPage = () => {
  return (
    <>
      {enabledOAuthProviders.map((provider) => {
        const ProviderIcon = providerIconById[provider.id]

        return (
          <a key={provider.id} href={provider.href}>
            {ProviderIcon && <ProviderIcon />}
            Sign in with {provider.label}
          </a>
        )
      })}
    </>
  );
};
```
