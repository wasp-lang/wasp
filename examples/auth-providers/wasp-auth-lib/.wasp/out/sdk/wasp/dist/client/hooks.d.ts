/**
 * Hook to avoid running effect twice when using React's `StrictMode` in development mode.
 * React renders each component twice in development mode to detect potential effect clean up
 * issues. Wasp relies on some effects triggering only once, for example, OAuth token exchange
 * or e-mail verification.
 */
export declare function useEffectOnce(callback: () => void): void;
//# sourceMappingURL=hooks.d.ts.map