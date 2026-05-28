import { LoginForm } from "wasp/client/auth";
import { Link as WaspRouterLink, routes } from "wasp/client/router";
import { AuthPageLayout } from "./AuthPageLayout";
import { useRedirectIfLoggedIn } from "./hooks/useRedirectIfLoggedIn";

export function Login() {
  useRedirectIfLoggedIn();

  return (
    <AuthPageLayout>
      <LoginForm />
      <br />
      <span className="text-sm font-medium text-gray-900 dark:text-gray-900">
        Don't have an account yet?{" "}
        <WaspRouterLink to={routes.SignupRoute.to} className="underline">
          Go to signup
        </WaspRouterLink>
        .
      </span>
      <br />
      <span className="text-sm font-medium text-gray-900">
        Forgot your password?{" "}
        <WaspRouterLink
          to={routes.RequestPasswordResetRoute.to}
          className="underline"
        >
          Reset it
        </WaspRouterLink>
        .
      </span>
    </AuthPageLayout>
  );
}
