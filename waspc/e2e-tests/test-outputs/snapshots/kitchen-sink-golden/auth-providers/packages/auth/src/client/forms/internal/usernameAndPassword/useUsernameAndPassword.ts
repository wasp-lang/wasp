import { login, signup } from "../../../actions.js";

export function useUsernameAndPassword({
  onError,
  onSuccess,
  isLogin,
}: {
  onError: (error: Error) => void;
  onSuccess: () => void;
  isLogin: boolean;
}) {
  async function handleSubmit(data: { username: string; password: string }) {
    try {
      if (!isLogin) {
        await signup(data);
      }
      await login(data);
      onSuccess();
    } catch (err: unknown) {
      onError(err as Error);
    }
  }
  return { handleSubmit };
}
