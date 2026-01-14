import { useEffect } from "react";
import CookieConsent, { Cookies } from "react-cookie-consent";
import { SCRIPT_WITH_CONSENT_TYPE } from "../lib/cookie-consent";
import classes from "./CookieConsentBanner.module.css";

const CONSENT_COOKIE_NAME = "wasp-cookies-consent";

export default function CookieConsentBanner() {
  useEffect(() => {
    if (Cookies.get(CONSENT_COOKIE_NAME) === "true") {
      loadScripts();
    }
  }, []);

  return (
    <CookieConsent
      location="none"
      buttonText="Accept all"
      declineButtonText="Reject all"
      enableDeclineButton
      onAccept={loadScripts}
      disableStyles={true}
      containerClasses={classes.container}
      contentClasses={classes.content}
      buttonWrapperClasses={classes.buttonWrapper}
      buttonClasses={classes.buttonAccept}
      declineButtonClasses={classes.buttonDecline}
      cookieName={CONSENT_COOKIE_NAME}
    >
      We use cookies primarily for analytics to enhance your experience. By
      accepting, you agree to our use of these cookies. You can manage your
      preferences or{" "}
      <a href="/privacy-policy">learn more about our cookie policy</a>.
    </CookieConsent>
  );
}

function loadScripts() {
  document
    .querySelectorAll(`script[type="${SCRIPT_WITH_CONSENT_TYPE}"]`)
    .forEach((oldScript) => {
      const newScript = document.createElement("script");
      Array.from(oldScript.attributes).forEach((attr) => {
        if (attr.name === "type") {
          newScript.setAttribute("type", "text/javascript");
        } else {
          newScript.setAttribute(attr.name, attr.value);
        }
      });
      oldScript.parentNode?.replaceChild(newScript, oldScript);
    });
}
