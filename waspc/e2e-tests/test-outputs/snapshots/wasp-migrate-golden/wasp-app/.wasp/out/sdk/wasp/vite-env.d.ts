/// <reference types="vite/client" />

interface WaspSSRData {
  isFallbackPage: boolean
  routerHydrationData?: import("react-router").HydrationState
}

interface Window {
  __WASP_SSR_DATA__?: WaspSSRData
}
