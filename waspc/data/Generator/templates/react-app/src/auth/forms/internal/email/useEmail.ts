import { signup } from '../../../email/actions/signup'
import { login } from '../../../email/actions/login'

export function useEmail({
  onError,
  showEmailVerificationPending,
  isEmailVerificationRequired,
  onLoginSuccess,
  isLogin,
}: {
  onError: (error: Error) => void
  showEmailVerificationPending: () => void
  onLoginSuccess: () => void
  isLogin: boolean
  isEmailVerificationRequired: boolean
}) {
  async function handleSubmit(data) {
    try {
      if (isLogin) {
        await login(data)
        onLoginSuccess()
      } else {
        await signup(data)
        if (isEmailVerificationRequired) {
          showEmailVerificationPending()
        } else {
          await login(data)
          onLoginSuccess()
        }
      }
    } catch (err: unknown) {
      onError(err as Error)
    }
  }

  return {
    handleSubmit,
  }
}
