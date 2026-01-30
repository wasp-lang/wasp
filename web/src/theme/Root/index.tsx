import CookieConsentBanner from "@site/src/components/CookieConsentBanner";

export default function Root({ children }) {
  return (
    <>
      {children}
      <CookieConsentBanner />
    </>
  );
}
