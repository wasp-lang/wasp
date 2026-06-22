import { useEffect } from "react";
import * as CookieConsent from "vanilla-cookieconsent";
import "vanilla-cookieconsent/dist/cookieconsent.css";
import { getConfig } from "./Config";

/**
 * NOTE: if you do not want to use the cookie consent banner, you should
 * run `npm uninstall vanilla-cookieconsent`, and delete this component, its config file,
 * as well as its import in src/client/App.tsx .
 */
export function CookieConsentBanner() {
  useEffect(() => {
    CookieConsent.run(getConfig());
  }, []);

  return <div id="cookieconsent"></div>;
}
