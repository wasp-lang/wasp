import { login, signup } from '../../../username'
import type { UsernameSignupData } from "../../../username/actions/signup";

// PRIVATE API
export function useUsernameAndPassword({
  onError,
  onSuccess,
  isLogin,
}: {
  onError: (error: unknown) => void
  onSuccess: () => void
  isLogin: boolean
}) {
  async function handleSubmit(data: UsernameSignupData): Promise<void> {
    try {
      if (!isLogin) {
        await signup(data)
      }
      await login(data)

      onSuccess()
    } catch (error: unknown) {
      onError(error);
    }
  }

  return {
    handleSubmit,
  }
}
