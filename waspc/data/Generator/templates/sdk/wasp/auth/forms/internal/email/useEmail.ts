import { signup } from '../../../email/actions/signup'
import { login } from '../../../email/actions/login'

// PRIVATE API
export function useEmail({
  onError,
  showEmailVerificationPending,
  onLoginSuccess,
  isLogin,
}: {
  onError: (error: Error) => void
  showEmailVerificationPending: () => void
  onLoginSuccess: () => void
  isLogin: boolean
}) {
  async function handleSubmit(data) {
    try {
      if (isLogin) {
        await login(data)
        onLoginSuccess()
      } else {
        await signup(data)
        showEmailVerificationPending()
      }
    } catch (err: unknown) {
      onError(err as Error)
    }
  }

  return {
    handleSubmit,
  }
}
