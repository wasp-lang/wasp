import type { PostHog } from "posthog-js";
import { useEffect, useState, type MouseEvent as ReactMouseEvent } from "react";
import { track, trackPlacementAttributes } from "../lib/analytics";
import {
  COOKIE_SETTINGS_HREF,
  fetchRequiresConsent,
} from "../lib/cookie-consent";
import {
  CONSENT_STORAGE_KEY,
  optInToPostHog,
  optOutOfPostHog,
  startPostHog,
} from "../lib/posthog";
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

    fetchRequiresConsent().then(async (requiresConsent) => {
      const posthog = await startPostHog(requiresConsent);
      setPostHog(posthog);
      // PostHog keeps the choice, so a visitor sees this banner only once.
      if (
        requiresConsent &&
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

  useEffect(() => {
    if (!posthog) {
      return;
    }

    // A choice in another tab changes the stored choice. PostHog in this tab
    // reads the stored choice before each event, but it also keeps its data in
    // memory, and it can save that data in the browser again. So this tab
    // makes the same choice. The browser sends this event only to the other
    // tabs, and only when the value changes, so it does not come back. An
    // event can arrive after a newer choice, so this tab applies only the
    // choice that is stored now.
    const applyChoiceFromOtherTab = (event: StorageEvent) => {
      if (
        event.key !== CONSENT_STORAGE_KEY ||
        event.newValue !== localStorage.getItem(CONSENT_STORAGE_KEY)
      ) {
        return;
      }
      if (event.newValue === "1") {
        optInToPostHog(posthog);
        setBannerState("closed");
      } else if (event.newValue === "0") {
        optOutOfPostHog(posthog);
        setBannerState("closed");
      }
    };

    window.addEventListener("storage", applyChoiceFromOtherTab);
    return () => window.removeEventListener("storage", applyChoiceFromOtherTab);
  }, [posthog]);

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
        We use PostHog to learn how people use this site. If you accept, PostHog
        stores an ID in your browser and records the pages you visit and the
        links you click. PostHog processes this data and your IP address in the
        United States. You can change your choice at any time with the "Cookie
        settings" link at the bottom of every page.{" "}
        <a href="/privacy-policy">Learn more in our privacy policy</a>.
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
