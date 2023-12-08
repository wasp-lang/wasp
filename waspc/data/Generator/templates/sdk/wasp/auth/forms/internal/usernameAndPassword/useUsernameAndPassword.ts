import { useState } from 'react'
import signup from '../../../signup'
import login from '../../../login'

export function useUsernameAndPassword({
  onError,
  onSuccess,
  isLogin,
}: {
  onError: (error: Error) => void
  onSuccess: () => void
  isLogin: boolean
}) {
  const [usernameFieldVal, setUsernameFieldVal] = useState('')
  const [passwordFieldVal, setPasswordFieldVal] = useState('')

  async function handleSubmit() {
    try {
      if (!isLogin) {
        await signup({
          username: usernameFieldVal,
          password: passwordFieldVal,
        })
      }
      await login(usernameFieldVal, passwordFieldVal)

      setUsernameFieldVal('')
      setPasswordFieldVal('')
      onSuccess()
    } catch (err: unknown) {
      onError(err as Error)
    }
  }

  return {
    handleSubmit,
    usernameFieldVal,
    passwordFieldVal,
    setUsernameFieldVal,
    setPasswordFieldVal,
  }
}
