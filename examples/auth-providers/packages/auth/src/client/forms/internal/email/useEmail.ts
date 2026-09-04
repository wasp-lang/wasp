import { login, signup } from "../../../actions.js";

export function useEmail({
  onError,
  showEmailVerificationPending,
  onLoginSuccess,
  isLogin,
}: {
  onError: (error: Error) => void;
  showEmailVerificationPending: () => void;
  onLoginSuccess: () => void;
  isLogin: boolean;
}) {
  async function handleSubmit(data: { email: string; password: string }) {
    try {
      if (isLogin) {
        await login(data);
        onLoginSuccess();
      } else {
        await signup(data);
        showEmailVerificationPending();
      }
    } catch (err: unknown) {
      onError(err as Error);
    }
  }
  return { handleSubmit };
}
