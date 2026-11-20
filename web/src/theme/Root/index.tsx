import CookieConsentBanner from "@site/src/components/CookieConsentBanner";
import type { ReactNode } from "react";

export default function Root({ children }: { children: ReactNode }) {
  return (
    <>
      {children}
      {/* PostHog runs only in a production build, the same as Plausible in
          `prodOnlyScripts` in `docusaurus.config.ts`. */}
      {process.env.NODE_ENV === "production" && <CookieConsentBanner />}
    </>
  );
}
