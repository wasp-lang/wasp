/// <reference types="vite/client" />

import type { HydrationState } from "react-router"

declare global {
  interface Window {
    __staticRouterHydrationData?: HydrationState;
  }
}
