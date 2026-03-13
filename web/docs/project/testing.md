---
title: Testing
---

import { GuideLink } from '@site/src/components/GuideLink';

Because Wasp uses [Vite](https://vitejs.dev/), you can use any testing tool that works with Vite. This includes unit tests, React component tests, and server-side tests.

## What Wasp provides

Wasp exports `WaspTestWrapper` from `wasp/client/test`. It wraps your components with the `QueryClientProvider` and `BrowserRouter` that Wasp components expect, so you can render them in tests without extra setup:

```tsx
import { render } from '@testing-library/react'
import { WaspTestWrapper } from 'wasp/client/test'

render(<MyComponent />, { wrapper: WaspTestWrapper })
```

The choice of testing framework, assertion library, and mocking tools is up to you.

We provide a step-by-step guide for setting up Vitest with some common testing libraries:

<GuideLink
  linkToGuide="../guides/libraries/vitest"
  title="Vitest"
  description="Set up Vitest for unit, component, and server-side tests in your Wasp project"
/>
