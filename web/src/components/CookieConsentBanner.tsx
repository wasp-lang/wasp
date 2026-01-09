import "@site/src/css/cookie-consent.css";
import { useEffect } from "react";
import CookieConsent, { Cookies } from "react-cookie-consent";

const consentCookieName = "wasp-cookies-consent";

export default function CookieConsentBanner() {
  const handleAccept = () => {
    loadScripts();
  };

  useEffect(() => {
    if (Cookies.get(consentCookieName) === "true") {
      loadScripts();
    }
  }, []);

  return (
    <CookieConsent
      location="none"
      buttonText="Accept all"
      declineButtonText="Reject all"
      enableDeclineButton
      onAccept={handleAccept}
      disableStyles={true}
      containerClasses="cookie-consent-container"
      contentClasses="cookie-consent-content"
      buttonWrapperClasses="cookie-consent-button-wrapper"
      buttonClasses="cookie-consent-button-accept"
      declineButtonClasses="cookie-consent-button-decline"
      cookieName={consentCookieName}
    >
      We use cookies primarily for analytics to enhance your experience. By
      accepting, you agree to our use of these cookies. You can manage your
      preferences or{" "}
      <a href="/privacy-policy">learn more about our cookie policy</a>.
    </CookieConsent>
  );
}

type Script = {
  name: string;
  load: () => void;
};

const scriptsToLoadAfterConsent: Script[] = [
  {
    name: "PostHog",
    load() {
      const script = document.createElement("script");
      script.src = "/scripts/posthog.js";
      script.defer = true;
      document.head.appendChild(script);
    },
  },
];

function loadScripts() {
  for (const script of scriptsToLoadAfterConsent) {
    script.load();
  }
}
