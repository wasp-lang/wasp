// prettier-ignore
!function(t,e){var o,n,p,r;e.__SV||(window.posthog=e,e._i=[],e.init=function(i,s,a){function g(t,e){var o=e.split(".");2==o.length&&(t=t[o[0]],e=o[1]),t[e]=function(){t.push([e].concat(Array.prototype.slice.call(arguments,0)))}}(p=t.createElement("script")).type="text/javascript",p.crossOrigin="anonymous",p.async=!0,p.src=s.api_host.replace(".i.posthog.com","-assets.i.posthog.com")+"/static/array.js",(r=t.getElementsByTagName("script")[0]).parentNode.insertBefore(p,r);var u=e;for(void 0!==a?u=e[a]=[]:a="posthog",u.people=u.people||[],u.toString=function(t){var e="posthog";return"posthog"!==a&&(e+="."+a),t||(e+=" (stub)"),e},u.people.toString=function(){return u.toString(1)+".people (stub)"},o="init capture register register_once register_for_session unregister unregister_for_session getFeatureFlag getFeatureFlagPayload isFeatureEnabled reloadFeatureFlags updateEarlyAccessFeatureEnrollment getEarlyAccessFeatures on onFeatureFlags onSessionId getSurveys getActiveMatchingSurveys renderSurvey canRenderSurvey getNextSurveyStep identify setPersonProperties group resetGroups setPersonPropertiesForFlags resetPersonPropertiesForFlags setGroupPropertiesForFlags resetGroupPropertiesForFlags reset get_distinct_id getGroups get_session_id get_session_replay_url alias set_config startSessionRecording stopSessionRecording sessionRecordingStarted captureException loadToolbar get_property getSessionProperty createPersonProfile opt_in_capturing opt_out_capturing has_opted_in_capturing has_opted_out_capturing clear_opt_in_out_capturing debug".split(" "),n=0;n<o.length;n++)g(u,o[n]);e._i.push([i,s,a])},e.__SV=1)}(document,window.posthog||[]);
posthog.init("CdDd2A0jKTI2vFAsrI9JWm3MqpOcgHz1bMyogAcwsE4", {
  api_host: "https://us.i.posthog.com",
  person_profiles: "identified_only",
  // Docusaurus uses client-side navigation, so page changes don't trigger a
  // full page load. This tells PostHog to capture a page view on each path
  // change.
  capture_pageview: "history_change",
  // The privacy policy does not name these features. These lines keep them
  // off, also when somebody turns them on in the PostHog project.
  disable_session_recording: true,
  disable_surveys: true,
  capture_dead_clicks: false,
  capture_exceptions: false,
  capture_performance: false,
  // The site reads no feature flags, and the PostHog project has none. Without
  // this, PostHog asks for flags every five minutes, also after an opt-out,
  // and each request sends the `distinct_id` and `$device_id` of the visitor.
  // `advanced_disable_flags` would also stop the remote config, and with it
  // the heatmaps.
  advanced_disable_feature_flags: true,
  // A browser with Do Not Track or Global Privacy Control counts as "Reject all".
  respect_dnt: true,
  // An opt-out also removes the PostHog cookie.
  opt_out_persistence_by_default: true,
  loaded: function (posthog) {
    // The visitor can click "Reject all" while this script downloads, also in
    // another tab. This is a copy of `getConsent()` and `CONSENT_COOKIE_NAME`
    // in `src/lib/cookie-consent.ts`, because a static script cannot import
    // them.
    var consentCookie = document.cookie.split("; ").find(function (cookie) {
      return cookie.startsWith("wasp-cookies-consent=");
    });
    var choice = consentCookie
      ? consentCookie.split("=")[1]
      : window.waspCookieConsent;
    if (choice === "false") {
      posthog.opt_out_capturing();
    } else if (
      choice === "true" &&
      posthog.get_explicit_consent_status() === "denied"
    ) {
      // A newer "Accept all" replaces the opt-out from an earlier "Reject
      // all". Without a choice, PostHog keeps its own stored opt-out, for
      // example when the browser could not store the consent cookie.
      // Plausible counts each choice with the "Cookie Consent" events, so
      // PostHog sends no `$opt_in` event. With Do Not Track the status is also
      // "denied", but PostHog still records nothing after the opt-in.
      posthog.opt_in_capturing({ captureEventName: false });
    }
  },
});
