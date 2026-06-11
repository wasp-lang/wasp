{{={= =}=}}
import { useState } from "react";
import { Navigate, useLocation } from 'react-router'
import { useAuth } from "../../auth";
import { api } from "../../../api";
import { initSession } from "../../../auth/helpers/user";
import { useEffectOnce } from "../../hooks";
import { MessageLoading, MessageError } from "../components/Message";
import { FullPageWrapper } from "../components/FullPageWrapper";

const oAuthCallbackWrapperClassName = "wasp-oauth-callback-wrapper";

export function OAuthCallbackPage() {
  const { error, user } = useOAuthCallbackHandler();

  if (user !== undefined && user !== null) {
    return <Navigate to="{= onAuthSucceededRedirectTo =}" replace />;
  }


  if (error) {
    return (
      <FullPageWrapper className={oAuthCallbackWrapperClassName}>
        <MessageError>{error}</MessageError>
      </FullPageWrapper>
    );
  }

  return (
    <FullPageWrapper className={oAuthCallbackWrapperClassName}>
      <MessageLoading>Please wait a moment while we log you in.</MessageLoading>
    </FullPageWrapper>
  );
}

function useOAuthCallbackHandler() {
  const { data: user, isLoading: isUserLoading } = useAuth();
  const [isCallbackLoading, setisCallbackLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);
  const location = useLocation();

  async function handleCallback() {
    try {
      setisCallbackLoading(true);
      const query = new URLSearchParams(location.search);

      // If we got redirect with an error, display it to the user
      // and don't continue with the login process.
      const errorFromRedirect = query.get('error');
      if (errorFromRedirect !== null) {
        setError(errorFromRedirect);
        return;
      }

      const code = location.hash.slice(1);
      const data = await exchangeOAuthCodeForToken({ code });
      if (!isDataWithSessionId(data)) {
        setError("Unable to login with the OAuth provider.");
        return;
      }
      await initSession(data.sessionId);
    } catch (e: unknown) {
      console.error(e);
      setError("Unable to login with the OAuth provider.");
    } finally {
      setisCallbackLoading(false);
    }
  }

  useEffectOnce(() => {
    handleCallback();
  });

  return {
    user,
    error,
    isCallbackLoading,
    isUserLoading,
  };
}

async function exchangeOAuthCodeForToken(data: {
  code: string
}): Promise<unknown> {
  return api.post('/auth/exchange-code', {
    json: data,
  }).json()
}

function isDataWithSessionId(
  data: unknown
): data is { sessionId: string } {
  const obj = data as any;
  return !!obj && typeof obj.sessionId === 'string'
}
