import type {
  EmailAuthActions,
  EmailSignupData,
} from "../../../actions/email.js";

export function useEmail({
  actions,
  onError,
  showEmailVerificationPending,
  onLoginSuccess,
  isLogin,
}: {
  actions: EmailAuthActions<object>;
  onError: (error: unknown) => void;
  showEmailVerificationPending: () => void;
  onLoginSuccess: () => void;
  isLogin: boolean;
}) {
  async function handleSubmit(data: EmailSignupData<object>): Promise<void> {
    try {
      if (isLogin) {
        await actions.login(data);
        onLoginSuccess();
      } else {
        await actions.signup(data);
        showEmailVerificationPending();
      }
    } catch (error: unknown) {
      onError(error);
    }
  }

  return {
    handleSubmit,
  };
}
