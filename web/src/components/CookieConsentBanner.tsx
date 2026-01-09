import "@site/src/css/cookie-consent.css";
import { useEffect } from "react";
import CookieConsent, { Cookies } from "react-cookie-consent";

const consentCookieName = "wasp-cookies-consent";

const scriptsToLoadAfterConsent: Script[] = [
  {
    name: "PostHog",
    load: () => loadScript("/scripts/posthog.js"),
  },
  {
    name: "REO.dev",
    async load() {
      const REO_CLIENT_ID = "96f303a127451b4";
      await loadScript(`https://static.reo.dev/${REO_CLIENT_ID}/reo.js`);
      window.Reo.init({ clientID: REO_CLIENT_ID });
    },
  },
];

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
  load: () => Promise<void>;
};

declare global {
  interface Window {
    Reo?: any;
  }
}

async function loadScript(src: string): Promise<void> {
  return new Promise((resolve) => {
    const script = document.createElement("script");
    script.src = src;
    script.defer = true;
    script.onload = () => resolve();
    document.head.appendChild(script);
  });
}

function loadScripts() {
  for (const script of scriptsToLoadAfterConsent) {
    script.load();
  }
}
