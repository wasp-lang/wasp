import type { PostHog } from "posthog-js";
import { useEffect, useState, type MouseEvent as ReactMouseEvent } from "react";
import { track, trackPlacementAttributes } from "../lib/analytics";
import {
  COOKIE_SETTINGS_HREF,
  isAnalyticsConsentRequired,
} from "../lib/cookie-consent";
import { optInToPostHog, optOutOfPostHog, startPostHog } from "../lib/posthog";
import classes from "./CookieConsentBanner.module.css";

// "banner": the banner shows by itself, because the visitor in the consent
// region did not choose yet. "settings": the visitor opened it with the
// "Cookie settings" link.
type BannerState = "closed" | "banner" | "settings";

export default function CookieConsentBanner() {
  const [posthog, setPostHog] = useState<PostHog>();
  const [bannerState, setBannerState] = useState<BannerState>("closed");

  useEffect(() => {
    // PostHog runs only in a production build, the same as Plausible in
    // `prodOnlyScripts` in `docusaurus.config.ts`.
    if (process.env.NODE_ENV !== "production") {
      return;
    }

    isAnalyticsConsentRequired().then(async (isConsentRequired) => {
      const posthog = await startPostHog(isConsentRequired);
      setPostHog(posthog);
      // PostHog keeps the choice, so a visitor sees this banner only once.
      if (
        isConsentRequired &&
        posthog.get_explicit_consent_status() === "pending"
      ) {
        setBannerState("banner");
      }
    });
  }, []);

  useEffect(() => {
    function openSettings(event: MouseEvent) {
      if (
        event.target instanceof Element &&
        event.target.closest(`a[href="${COOKIE_SETTINGS_HREF}"]`)
      ) {
        event.preventDefault();
        setBannerState("settings");
      }
    }

    document.addEventListener("click", openSettings);
    return () => document.removeEventListener("click", openSettings);
  }, []);

  if (!posthog || bannerState === "closed") {
    return null;
  }

  const handleAccept = (event: ReactMouseEvent<HTMLButtonElement>) => {
    optInToPostHog(posthog);
    track(event.currentTarget, "Cookie Consent: Accept", {});
    setBannerState("closed");
  };

  const handleReject = (event: ReactMouseEvent<HTMLButtonElement>) => {
    // The opt-out comes first, so PostHog drops the "Reject" event.
    optOutOfPostHog(posthog);
    track(event.currentTarget, "Cookie Consent: Reject", {});
    setBannerState("closed");
  };

  return (
    <section
      aria-label="Cookie consent"
      {...trackPlacementAttributes(
        bannerState === "settings" ? "cookie_settings" : "cookie_banner",
      )}
      // `ph-no-capture` turns off PostHog autocapture in the banner. PostHog
      // records a click before our handlers run, so without it PostHog sends
      // the click on "Reject all" after the opt-out.
      className={`ph-no-capture ${classes.container}`}
    >
      <p className={classes.content}>
        We use PostHog for analytics. If you accept, it stores an ID in your
        browser and records your visits and clicks. PostHog processes this data
        and your IP address in the US. You can change this in "Cookie settings"
        at any time. <a href="/privacy-policy">Privacy policy</a>
      </p>
      <div className={classes.buttonWrapper}>
        <button
          type="button"
          className={classes.buttonDecline}
          onClick={handleReject}
        >
          Reject all
        </button>
        <button
          type="button"
          className={classes.buttonAccept}
          onClick={handleAccept}
        >
          Accept all
        </button>
      </div>
    </section>
  );
}
