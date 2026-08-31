{{={= =}=}}
{=# usesSessionHandoff =}
import { useState } from "react";
{=/ usesSessionHandoff =}
import { Navigate, useLocation } from 'react-router'
import { useAuth } from "../../auth";
{=# usesSessionHandoff =}
import { api } from "../../../api";
import { initSession } from "../../../auth/helpers/user";
import { useEffectOnce } from "../../hooks";
{=/ usesSessionHandoff =}
import { MessageLoading, MessageError } from "../components/Message";
import { FullPageWrapper } from "../components/FullPageWrapper";
{=# usesSessionHandoff =}
import { browserAppDelivery } from "../../index";
{=/ usesSessionHandoff =}

const oauthLoginResultWrapperClassName = "wasp-oauth-login-result-wrapper";

export function OAuthLoginResultPage() {
  const { error, user, isLoading } = useOAuthLoginResult();

  if (user !== undefined && user !== null) {
    return <Navigate to="{= onAuthSucceededRedirectTo =}" replace />;
  }


  if (error) {
    return (
      <FullPageWrapper className={oauthLoginResultWrapperClassName}>
        <MessageError>{error}</MessageError>
      </FullPageWrapper>
    );
  }

  if (!isLoading && user === null) {
    return (
      <FullPageWrapper className={oauthLoginResultWrapperClassName}>
        <MessageError>Unable to login with the OAuth provider.</MessageError>
      </FullPageWrapper>
    )
  }

  return (
    <FullPageWrapper className={oauthLoginResultWrapperClassName}>
      <MessageLoading>Please wait a moment while we log you in.</MessageLoading>
    </FullPageWrapper>
  );
}

function useOAuthLoginResult() {
  const { data: user, isLoading: isUserLoading } = useAuth();
  {=# usesSessionHandoff =}
  const [isSessionHandoffLoading, setIsSessionHandoffLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);
  {=/ usesSessionHandoff =}
  const location = useLocation();

  {=# usesSessionHandoff =}
  async function exchangeSessionHandoff() {
    try {
      setIsSessionHandoffLoading(true);
      const query = new URLSearchParams(location.search);

      // If we got redirect with an error, display it to the user
      // and don't continue with the login process.
      const errorFromRedirect = query.get('error');
      if (errorFromRedirect !== null) {
        setError(errorFromRedirect);
        return;
      }

      const sessionHandoffCode = location.hash.slice(1);
      const data = await exchangeSessionHandoffCode(sessionHandoffCode);
      if (!hasSessionId(data)) {
        setError("Unable to login with the OAuth provider.");
        return;
      }
      await initSession(data.sessionId);
    } catch (e: unknown) {
      console.error(e);
      setError("Unable to login with the OAuth provider.");
    } finally {
      setIsSessionHandoffLoading(false);
    }
  }

  useEffectOnce(() => {
    exchangeSessionHandoff();
  });
  {=/ usesSessionHandoff =}

  {=^ usesSessionHandoff =}
  const errorFromRedirect = new URLSearchParams(location.search).get('error')
  {=/ usesSessionHandoff =}

  return {
    user,
    {=# usesSessionHandoff =}
    error,
    isLoading: isSessionHandoffLoading || isUserLoading,
    {=/ usesSessionHandoff =}
    {=^ usesSessionHandoff =}
    error: errorFromRedirect,
    isLoading: isUserLoading,
    {=/ usesSessionHandoff =}
  };
}{=# usesSessionHandoff =}

async function exchangeSessionHandoffCode(sessionHandoffCode: string): Promise<unknown> {
  return api.post(browserAppDelivery.waspApiPath('{= sessionHandoffExchangeRoute =}'), {
    json: { sessionHandoffCode },
  }).json()
}

function hasSessionId(
  data: unknown
): data is { sessionId: string } {
  const obj = data as any;
  return !!obj && typeof obj.sessionId === 'string'
}{=/ usesSessionHandoff =}
