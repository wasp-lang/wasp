import type { PostHog } from "posthog-js";
import { useEffect, useState, type MouseEvent as ReactMouseEvent } from "react";
import { track, trackPlacementAttributes } from "../lib/analytics";
import {
  COOKIE_SETTINGS_ATTRIBUTE,
  isAnalyticsConsentRequired,
} from "../lib/cookie-consent";
import { optInToPostHog, optOutOfPostHog, startPostHog } from "../lib/posthog";
import classes from "./CookieConsentBanner.module.css";

enum BannerState {
  Closed,
  // The banner shows by itself, because the visitor in the consent region did
  // not choose yet.
  Banner,
  // The visitor opened the banner with the "Cookie settings" button.
  Settings,
}

export default function CookieConsentBanner() {
  const [posthog, setPostHog] = useState<PostHog>();
  const [bannerState, setBannerState] = useState(BannerState.Closed);

  useEffect(() => {
    async function start() {
      const isConsentRequired = await isAnalyticsConsentRequired();
      const posthog = await startPostHog(isConsentRequired);
      setPostHog(posthog);
      // PostHog keeps the choice, so a visitor sees this banner only once.
      if (
        isConsentRequired &&
        posthog.get_explicit_consent_status() === "pending"
      ) {
        setBannerState(BannerState.Banner);
      }
    }

    start().catch(() => {
      // PostHog did not load, for example because of a network error. The site
      // works without PostHog, so the banner stays closed.
    });
  }, []);

  useEffect(() => {
    // The footer in `docusaurus.config.ts` is plain HTML, so one listener on
    // the page handles the "Cookie settings" buttons of both footers.
    function openSettings(event: MouseEvent) {
      if (
        event.target instanceof Element &&
        event.target.closest(`[${COOKIE_SETTINGS_ATTRIBUTE}]`)
      ) {
        setBannerState(BannerState.Settings);
      }
    }

    document.addEventListener("click", openSettings);
    return () => document.removeEventListener("click", openSettings);
  }, []);

  if (!posthog || bannerState === BannerState.Closed) {
    return null;
  }

  const handleAccept = (event: ReactMouseEvent<HTMLButtonElement>) => {
    optInToPostHog(posthog);
    track(event.currentTarget, "Cookie Consent: Accept", {});
    setBannerState(BannerState.Closed);
  };

  const handleReject = (event: ReactMouseEvent<HTMLButtonElement>) => {
    // The opt-out comes first, so PostHog drops the "Reject" event.
    optOutOfPostHog(posthog);
    track(event.currentTarget, "Cookie Consent: Reject", {});
    setBannerState(BannerState.Closed);
  };

  return (
    <section
      aria-label="Cookie consent"
      {...trackPlacementAttributes(
        bannerState === BannerState.Settings
          ? "cookie_settings"
          : "cookie_banner",
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
