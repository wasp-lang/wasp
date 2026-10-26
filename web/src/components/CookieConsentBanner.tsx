import { useEffect, useRef, useState } from "react";
import CookieConsent, {
  Cookies,
  VISIBILITY_OPTIONS,
} from "react-cookie-consent";
import { track, trackPlacementAttributes } from "../lib/analytics";
import {
  CONSENT_COOKIE_NAME,
  COOKIE_SETTINGS_HREF,
  deletePostHogStorage,
  fetchRequiresConsent,
  getConsent,
  SCRIPT_WITH_CONSENT_TYPE,
} from "../lib/cookie-consent";
import classes from "./CookieConsentBanner.module.css";

// Each "Reject all" goes to the other open tabs on this channel.
const REJECT_CHANNEL_NAME = "wasp-cookies-reject";

// `loadScripts()` sets it. Then "Reject all" must reload the page, because only
// a reload stops a script that already runs.
let areScriptsLoaded = false;

export default function CookieConsentBanner() {
  // `undefined` until the region check answers, and when the visitor already chose.
  const [requiresConsent, setRequiresConsent] = useState<boolean>();
  const [isSettingsOpen, setIsSettingsOpen] = useState(false);
  const bannerRef = useRef<HTMLDivElement>(null);
  const rejectChannelRef = useRef<BroadcastChannel | null>(null);

  useEffect(() => {
    const consent = getConsent();
    if (consent === "true") {
      loadScripts();
      return;
    }
    if (consent === "false") {
      // A "Reject all" can leave old PostHog data: an ad blocker can stop the
      // PostHog script, or the reload can come before PostHog deletes its
      // cookie. After the rejection, the script never loads again.
      deletePostHogStorage();
      return;
    }

    fetchRequiresConsent().then((requires) => {
      // Read the choice again: the visitor can choose before the answer, in
      // "Cookie settings" or in another tab.
      if (getConsent() !== undefined) {
        return;
      }
      setRequiresConsent(requires);
      // Outside the consent region, PostHog starts without a choice.
      if (!requires) {
        loadScripts();
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
        setIsSettingsOpen(true);
      }
    }

    document.addEventListener("click", openSettings);
    return () => document.removeEventListener("click", openSettings);
  }, []);

  useEffect(() => {
    // Old browsers have no `BroadcastChannel`, and the banner must work there too.
    if (typeof BroadcastChannel === "undefined") {
      return;
    }

    const channel = new BroadcastChannel(REJECT_CHANNEL_NAME);
    channel.onmessage = () => {
      // A late message must not undo a newer "Accept all". Only the shared
      // cookie can show a newer "Accept all", so with blocked cookies the
      // rejection wins.
      if (Cookies.get(CONSENT_COOKIE_NAME) !== "true") {
        window.waspCookieConsent = "false";
        // The visitor chose in the other tab, so this tab closes its banner.
        setIsSettingsOpen(false);
        setRequiresConsent(undefined);
        stopPostHog();
        removePostHog();
      }
    };
    rejectChannelRef.current = channel;
    return () => channel.close();
  }, []);

  function handleAccept() {
    window.waspCookieConsent = "true";
    loadScripts();
    // PostHog can already run on this page with an old opt-out, for example
    // after the consent cookie expired. The stub returns nothing for this
    // check, so only a loaded PostHog opts in here. Plausible counts each
    // choice with the "Cookie Consent" events, so PostHog sends no `$opt_in`
    // event.
    const posthog = window.posthog;
    if (posthog?.has_opted_out_capturing?.()) {
      posthog.opt_in_capturing?.({ captureEventName: false });
    }
    trackChoice("Cookie Consent: Accept");
    setIsSettingsOpen(false);
  }

  function handleDecline() {
    window.waspCookieConsent = "false";
    // The opt-out comes first, so PostHog does not get the "Reject" event.
    stopPostHog();
    trackChoice("Cookie Consent: Reject");
    rejectChannelRef.current?.postMessage(null);
    setIsSettingsOpen(false);
    removePostHog();
  }

  function trackChoice(
    eventName: "Cookie Consent: Accept" | "Cookie Consent: Reject",
  ) {
    if (bannerRef.current) {
      track(bannerRef.current, eventName, {});
    }
  }

  return (
    <div
      ref={bannerRef}
      {...trackPlacementAttributes(
        isSettingsOpen ? "cookie_settings" : "cookie_banner",
      )}
      // `ph-no-capture` turns off PostHog autocapture in the banner. PostHog
      // records a click before our handlers run, so without it PostHog sends
      // the click on "Reject all" when the page reloads, after the opt-out.
      className="ph-no-capture contents"
    >
      <CookieConsent
        location="none"
        visible={getVisibility(isSettingsOpen, requiresConsent)}
        buttonText="Accept all"
        declineButtonText="Reject all"
        // Without these, screen readers read the library defaults "Accept
        // cookies" and "Decline cookies", not the visible text.
        ariaAcceptLabel="Accept all"
        ariaDeclineLabel="Reject all"
        enableDeclineButton
        onAccept={handleAccept}
        onDecline={handleDecline}
        disableStyles={true}
        containerClasses={classes.container}
        contentClasses={classes.content}
        buttonWrapperClasses={classes.buttonWrapper}
        buttonClasses={classes.buttonAccept}
        declineButtonClasses={classes.buttonDecline}
        cookieName={CONSENT_COOKIE_NAME}
      >
        We use cookies primarily for analytics to enhance your experience. By
        accepting, you agree to our use of these cookies. You can change your
        choice at any time with the "Cookie settings" link at the bottom of
        every page.{" "}
        <a href="/privacy-policy">Learn more about our cookie policy</a>.
      </CookieConsent>
    </div>
  );
}

// The opt-out also stops the `$pageleave` event that PostHog sends when the
// page closes, for example at the reload in `removePostHog()`.
function stopPostHog() {
  window.posthog?.opt_out_capturing?.();
}

// After the reload, the stored choice stops PostHog from loading, so PostHog
// does not run on the page any more.
function removePostHog() {
  deletePostHogStorage();
  if (areScriptsLoaded) {
    window.location.reload();
  }
}

function getVisibility(
  isSettingsOpen: boolean,
  requiresConsent: boolean | undefined,
): string {
  if (isSettingsOpen) {
    return VISIBILITY_OPTIONS.SHOW;
  }
  if (requiresConsent) {
    return VISIBILITY_OPTIONS.BY_COOKIE_VALUE;
  }
  return VISIBILITY_OPTIONS.HIDDEN;
}

function loadScripts() {
  document
    .querySelectorAll(`script[type="${SCRIPT_WITH_CONSENT_TYPE}"]`)
    .forEach((el) => {
      // We need to reattach the element so that the browser picks up the new `type`.
      const parent = el.parentNode;
      if (!parent) {
        return;
      }
      parent.removeChild(el);
      el.setAttribute("type", "text/javascript");
      parent.appendChild(el);
      areScriptsLoaded = true;
    });
}
