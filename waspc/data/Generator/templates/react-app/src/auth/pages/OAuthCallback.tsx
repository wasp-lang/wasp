{{={= =}=}}
import { useEffect, useRef, useState } from "react";
import { type AxiosResponse } from "axios";
import { Redirect, useLocation } from 'react-router-dom'
import { useAuth } from 'wasp/client/auth'
import { api } from 'wasp/client/api'
import { initSession } from 'wasp/auth/helpers/user'

const wrapperStyles = {
  display: "flex",
  alignItems: "center",
  justifyContent: "center",
  padding: "4rem",
};

const commonMessageStyles = {
  display: 'flex',
  alignItems: 'center',
  gap: '.5rem',
  borderRadius: '.5rem',
  padding: '1rem',
};

const errorMessageStyles = {
  ...commonMessageStyles,
  borderColor: 'rgb(240 82 82)',
  backgroundColor: 'rgb(253 232 232)',
  color: 'rgb(200 30 30)',
};

const loadingMessageStyles = {
  ...commonMessageStyles,
  borderColor: 'rgb(107 114 128)',
  backgroundColor: 'rgb(243 244 246)',
  color: 'rgb(55 65 81)',
};

export function OAuthCallbackPage() {
  const { isLoading, error, user } = useOAuthCallbackHandler();
  
  if (user !== undefined && user !== null) {
    return <Redirect to="{= onAuthSucceededRedirectTo =}" />;
  }

  return (
    <div style={wrapperStyles}>
      {error && <div style={errorMessageStyles}><MessageIcon /> {error}</div>}
      {isLoading && <div style={loadingMessageStyles}><MessageIcon /> Please wait a moment while we log you in.</div>}
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

const MessageIcon = () => (
  <svg
    xmlns="http://www.w3.org/2000/svg"
    width="1.25rem"
    height="1.25rem"
    fill="currentColor"
    stroke="currentColor"
    strokeWidth={0}
    aria-hidden="true"
    viewBox="0 0 20 20"
  >
    <path
      fillRule="evenodd"
      stroke="none"
      d="M18 10a8 8 0 1 1-16 0 8 8 0 0 1 16 0zm-7-4a1 1 0 1 1-2 0 1 1 0 0 1 2 0zM9 9a1 1 0 0 0 0 2v3a1 1 0 0 0 1 1h1a1 1 0 1 0 0-2v-3a1 1 0 0 0-1-1H9z"
      clipRule="evenodd"
    />
  </svg>
)

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
