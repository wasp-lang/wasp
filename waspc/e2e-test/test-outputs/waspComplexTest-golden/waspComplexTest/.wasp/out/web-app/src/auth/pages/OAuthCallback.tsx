import { useEffect, useRef, useState } from "react";
import { type AxiosResponse } from "axios";
import { Navigate, useLocation } from 'react-router-dom'
import { useAuth } from 'wasp/client/auth'
import { api } from 'wasp/client/api'
import { initSession } from 'wasp/auth/helpers/user'
import { MessageLoading, MessageError } from "../../components/Message";
import { FullPageWrapper } from "../../components/FullPageWrapper";

const oAuthCallbackWrapperClassName = "wasp-oauth-callback-wrapper";

export function OAuthCallbackPage() {
  const { error, user } = useOAuthCallbackHandler();
  
  if (user !== undefined && user !== null) {
    return <Navigate to="/" replace />;
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
      const response = await exchangeOAuthCodeForToken({ code });
      if (!isResponseWithSessionId(response)) {
        setError("Unable to login with the OAuth provider.");
        return;
      }
      await initSession(response.data.sessionId);
    } catch (e: unknown) {
      console.error(e);
      setError("Unable to login with the OAuth provider.");
    } finally {
      setisCallbackLoading(false);
    }
  }

  const isFirstRender = useRef(true);
  useEffect(() => {
    if (isFirstRender.current) {
      isFirstRender.current = false;
      handleCallback();
    }
  }, []);

  return {
    user,
    error,
    isCallbackLoading,
    isUserLoading,
  };
}

async function exchangeOAuthCodeForToken(data: {
  code: string
}): Promise<AxiosResponse<unknown>> {
  return api.post('/auth/exchange-code', data)
}

function isResponseWithSessionId(
  response: AxiosResponse<unknown>
): response is AxiosResponse<{ sessionId: string }> {
  return response.data && typeof (response.data as any).sessionId === 'string'
}
