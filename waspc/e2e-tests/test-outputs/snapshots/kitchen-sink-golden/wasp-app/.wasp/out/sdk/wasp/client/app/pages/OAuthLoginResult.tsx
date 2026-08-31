import { Navigate, useLocation } from 'react-router'
import { useAuth } from "../../auth";
import { MessageLoading, MessageError } from "../components/Message";
import { FullPageWrapper } from "../components/FullPageWrapper";

const oauthLoginResultWrapperClassName = "wasp-oauth-login-result-wrapper";

export function OAuthLoginResultPage() {
  const { error, user, isLoading } = useOAuthLoginResult();

  if (user !== undefined && user !== null) {
    return <Navigate to="/" replace />;
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
  const location = useLocation();


  const errorFromRedirect = new URLSearchParams(location.search).get('error')

  return {
    user,
    error: errorFromRedirect,
    isLoading: isUserLoading,
  };
}
