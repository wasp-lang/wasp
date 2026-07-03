import {
  redirectToOriginalRoute,
  type OnAuthSucceededRedirectFn,
} from "wasp/client/auth";

export const onAuthSucceededRedirect: OnAuthSucceededRedirectFn =
  redirectToOriginalRoute({ fallback: "/" });
