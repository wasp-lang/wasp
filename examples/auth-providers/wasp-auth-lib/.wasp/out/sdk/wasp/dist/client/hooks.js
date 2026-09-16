import { useRef, useEffect } from 'react';
// PRIVATE API (sdk, web-app)
/**
 * Hook to avoid running effect twice when using React's `StrictMode` in development mode.
 * React renders each component twice in development mode to detect potential effect clean up
 * issues. Wasp relies on some effects triggering only once, for example, OAuth token exchange
 * or e-mail verification.
 */
export function useEffectOnce(callback) {
    const hasRun = useRef(false);
    useEffect(() => {
        if (!hasRun.current) {
            callback();
            hasRun.current = true;
        }
    }, [callback]);
}
//# sourceMappingURL=hooks.js.map