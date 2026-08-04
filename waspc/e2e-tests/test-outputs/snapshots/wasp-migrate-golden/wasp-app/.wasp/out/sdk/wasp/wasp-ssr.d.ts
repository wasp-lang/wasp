interface WaspSSRData {
  isFallbackPage: boolean
}

interface Window {
  __WASP_SSR_DATA__?: WaspSSRData
  __staticRouterHydrationData?: import("react-router").HydrationState
}
