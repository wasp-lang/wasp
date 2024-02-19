import signup from '../../../signup'
import login from '../../../login'

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
      await login(data.username, data.password)

      onSuccess()
    } catch (err: unknown) {
      onError(err as Error)
    }
  }

  return {
    handleSubmit,
  }
}
