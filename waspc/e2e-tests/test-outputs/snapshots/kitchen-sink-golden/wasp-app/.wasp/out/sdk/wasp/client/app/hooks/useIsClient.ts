import { useEffect, useState } from "react"

/**
 * Returns `true` if the component is running on the client (browser) and
 * `false` if on the server (SSR). Doesn't cause hydration mismatches, so it is
 * safe to use for conditional rendering.
 */
export function useIsClient() {
  const [isClient, setIsClient] = useState(false);

  useEffect(() => {
    setIsClient(true);
  }, []);

  return isClient;
}
