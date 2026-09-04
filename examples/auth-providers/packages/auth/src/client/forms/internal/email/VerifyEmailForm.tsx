import { useAuthContext } from "@wasp.sh/lib-auth/browser";
import { useLocation } from "react-router";

import { verifyEmail } from "../../../actions.js";
import { useEffectOnce } from "../../../hooks.js";
import { Message } from "../Message.js";

export const VerifyEmailForm = () => {
  const { isLoading, setErrorMessage, setSuccessMessage, setIsLoading } =
    useAuthContext();
  const location = useLocation();
  const token = new URLSearchParams(location.search).get("token");

  async function submitForm() {
    if (!token) {
      setErrorMessage({
        title:
          "The token is missing from the URL. Please check the link you received in your email.",
      });
      return;
    }
    setIsLoading(true);
    setErrorMessage(null);
    setSuccessMessage(null);
    try {
      await verifyEmail({ token });
      setSuccessMessage("Your email has been verified. You can now log in.");
    } catch (error) {
      const e = error as Error & { data?: { data?: { message?: string } } };
      setErrorMessage({ title: e.message, description: e.data?.data?.message });
    } finally {
      setIsLoading(false);
    }
  }

  useEffectOnce(() => {
    submitForm();
  });

  return <>{isLoading && <Message>Verifying email...</Message>}</>;
};
