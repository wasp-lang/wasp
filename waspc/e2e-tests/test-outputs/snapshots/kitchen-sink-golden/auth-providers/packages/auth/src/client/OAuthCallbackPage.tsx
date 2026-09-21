import { useState } from "react";
import { Navigate, useLocation } from "react-router";

import { confirmMerge, exchangeOAuthCodeForSession } from "./actions.js";
import { Message, MessageError } from "./forms/internal/Message.js";
import { useEffectOnce } from "./hooks.js";
import { getClientRuntime, getClientSpec } from "./runtime.js";

/**
 * The client route the OAuth handback lands on: redeems the one-time code,
 * then redirects. After an account-linking flow (`startOAuthLink`) there is
 * no code to redeem: the user is refreshed and sent to `linkedRedirectTo`.
 */
export function OAuthCallbackPage({
  linkedRedirectTo,
}: {
  /** Where to go after a provider was connected. Default: `onAuthSucceededRedirectTo`. */
  linkedRedirectTo?: string;
} = {}) {
  const [error, setError] = useState<string | null>(null);
  const [isDone, setIsDone] = useState(false);
  const [isLinked, setIsLinked] = useState(false);
  const [mergeTicket, setMergeTicket] = useState<string | null>(null);
  const location = useLocation();

  useEffectOnce(() => {
    (async () => {
      const query = new URLSearchParams(location.search);
      const errorFromRedirect = query.get("error");
      if (errorFromRedirect !== null) {
        setError(errorFromRedirect);
        return;
      }
      // The provider account belongs to another account of this user: the
      // merge needs their explicit yes, so nothing happens until they click.
      const ticket = query.get("mergeTicket");
      if (ticket !== null) {
        setMergeTicket(ticket);
        return;
      }
      if (query.get("linked") !== null) {
        await getClientRuntime().refreshUser();
        setIsLinked(true);
        return;
      }
      try {
        await exchangeOAuthCodeForSession(location.hash.slice(1));
        setIsDone(true);
      } catch (e) {
        console.error(e);
        setError("Unable to login with the OAuth provider.");
      }
    })();
  });

  if (mergeTicket !== null && !isLinked) {
    return (
      <Message>
        That account already belongs to another account here. Merge it into the
        one you are signed in to? Its data moves here and it is deleted.{" "}
        <button
          onClick={async () => {
            try {
              await confirmMerge(mergeTicket);
              setIsLinked(true);
            } catch (e) {
              console.error(e);
              setMergeTicket(null);
              setError("Merging the accounts failed.");
            }
          }}
        >
          Merge accounts
        </button>
      </Message>
    );
  }
  if (isLinked) {
    return (
      <Navigate
        to={linkedRedirectTo ?? getClientSpec().onAuthSucceededRedirectTo}
        replace
      />
    );
  }
  if (isDone) {
    return <Navigate to={getClientSpec().onAuthSucceededRedirectTo} replace />;
  }
  if (error) {
    return <MessageError>{error}</MessageError>;
  }
  return <Message>Please wait a moment while we log you in.</Message>;
}
