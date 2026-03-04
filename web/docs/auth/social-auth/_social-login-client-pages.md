import TailwindNote from "../_tailwind-note.md"

<TailwindNote />

Let's create a `auth.{jsx,tsx}` file in the `src/pages` folder and add the following to it:

```tsx title="src/pages/auth.tsx" auto-js
import type { ReactNode } from 'react'
import { LoginForm } from 'wasp/client/auth'

export function Login() {
  return (
    <Layout>
      <LoginForm />
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

We imported the generated Auth UI components and used them in our pages. Read more about the Auth UI components [here](../../auth/ui).
