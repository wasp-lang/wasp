import { useEffect, useRef, useState } from "react";
import { type AxiosResponse } from "axios";
import { Redirect, useLocation } from 'react-router-dom'
import { useAuth } from 'wasp/client/auth'
import { api } from 'wasp/client/api'
import { initSession } from 'wasp/auth/helpers/user'
import { MessageLoading, MessageError } from "../../components/Message";

const wrapperStyles = {
  display: 'flex',
  alignItems: 'center',
  justifyContent: 'center',
  padding: '4rem',
}

export function OAuthCallbackPage() {
  const { isLoading, error, user } = useOAuthCallbackHandler();
  
  if (user !== undefined && user !== null) {
    return <Redirect to="/" />;
  }

  return (
    <div style={wrapperStyles}>
      {error && <MessageError>{error}</MessageError>}
      {isLoading && <MessageLoading>Please wait a moment while we log you in.</MessageLoading>}
    </div>
  );
}

function useOAuthCallbackHandler() {
  const { data: user } = useAuth();
  const [isLoading, setIsLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);
  const location = useLocation();

  async function handleCallback() {
    try {
      setIsLoading(true);
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
      setIsLoading(false);
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
    isLoading,
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
