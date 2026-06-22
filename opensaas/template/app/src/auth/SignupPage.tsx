import { SignupForm } from "wasp/client/auth";
import { Link as WaspRouterLink, routes } from "wasp/client/router";
import { AuthPageLayout } from "./AuthPageLayout";
import { useRedirectIfLoggedIn } from "./hooks/useRedirectIfLoggedIn";

export function SignupPage() {
  useRedirectIfLoggedIn();

  return (
    <AuthPageLayout>
      <SignupForm />
      <br />
      <span className="text-sm font-medium text-gray-900">
        I already have an account (
        <WaspRouterLink to={routes.LoginRoute.to} className="underline">
          go to login
        </WaspRouterLink>
        ).
      </span>
      <br />
    </AuthPageLayout>
  );
}
