import type {
  UsernameAuthActions,
  UsernameSignupData,
} from "../../../actions/username.js";

export function useUsernameAndPassword({
  actions,
  onError,
  onSuccess,
  isLogin,
}: {
  actions: UsernameAuthActions<object>;
  onError: (error: unknown) => void;
  onSuccess: () => void;
  isLogin: boolean;
}) {
  async function handleSubmit(data: UsernameSignupData<object>): Promise<void> {
    try {
      if (!isLogin) {
        await actions.signup(data);
      }
      await actions.login(data);

      onSuccess();
    } catch (error: unknown) {
      onError(error);
    }
  }

  return {
    handleSubmit,
  };
}
