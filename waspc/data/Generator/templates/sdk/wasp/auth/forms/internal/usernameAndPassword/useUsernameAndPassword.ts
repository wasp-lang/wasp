import { login, signup } from '../../../username'

// PRIVATE API
export function useUsernameAndPassword({
  onError,
  onSuccess,
  isLogin,
}: {
  onError: (error: Error) => void
  onSuccess: () => void
  isLogin: boolean
}) {
  async function handleSubmit(data) {
    try {
      if (!isLogin) {
        await signup(data)
      }
      await login(data)

      onSuccess()
    } catch (err: unknown) {
      onError(err as Error)
    }
  }

  return {
    handleSubmit,
  }
}
