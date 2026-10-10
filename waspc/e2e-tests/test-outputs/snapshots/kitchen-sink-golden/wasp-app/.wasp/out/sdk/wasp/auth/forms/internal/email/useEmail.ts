import { signup } from '../../../email/actions/signup'
import { login } from '../../../email/actions/login'

// PRIVATE API
export function useEmail({
  onError,
  showEmailVerificationPending,
  onLoginSuccess,
  isLogin,
}: {
  onError: (error: unknown) => void
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
    } catch (error) {
      onError(error)
    }
  }

  return {
    handleSubmit,
  }
}
