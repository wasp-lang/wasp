// Set by the Plausible script in `getScripts()` in `docusaurus.config.ts`.
interface Window {
  plausible?: (
    eventName: string,
    options?: { props?: Record<string, string>; interactive?: boolean },
  ) => void;
}
