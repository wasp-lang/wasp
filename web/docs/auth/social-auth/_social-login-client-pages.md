import TailwindNote from "../_tailwind-note.md"

<TailwindNote />

Let's create a `auth.{jsx,tsx}` file in the `src/pages` folder and add the following to it:

```tsx title="src/pages/auth.tsx" auto-js
import type { ReactNode } from 'react'
import { enabledOAuthProviders, providerIconById } from 'wasp/client/auth'

export function LoginPage() {
  return (
    <Layout>
      <div className="space-y-3">
        {enabledOAuthProviders.map((provider) => {
          const ProviderIcon = providerIconById[provider.id]
          return (
            <a
              key={provider.id}
              href={provider.href}
              className="flex items-center justify-center gap-2 rounded-md border border-gray-300 px-3 py-2"
            >
              {ProviderIcon && <ProviderIcon className="h-5 w-5" />}
              Continue with {provider.label}
            </a>
          )
        })}
      </div>
    </Layout>
  )
}

// A layout component to center the content
export function Layout({ children }: { children: ReactNode }) {
  return (
    <div className="h-full w-full bg-white">
      <div className="flex min-h-[75vh] min-w-full items-center justify-center">
        <div className="h-full w-full max-w-sm bg-white p-5">
          <div>{children}</div>
        </div>
      </div>
    </div>
  )
}
```

The `enabledOAuthProviders` list is generated from your `main.wasp.ts` auth methods. Read more about the drop-in Auth UI options [here](../../auth/ui).
