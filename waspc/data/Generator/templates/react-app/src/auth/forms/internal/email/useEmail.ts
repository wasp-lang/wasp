import { useState } from 'react'
import { signup } from '../../../email/actions/signup.js'
import { login } from '../../../email/actions/login.js'

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
  const [emailFieldVal, setEmailFieldVal] = useState('')
  const [passwordFieldVal, setPasswordFieldVal] = useState('')

  async function handleSubmit() {
    try {
      if (isLogin) {
        await login({ email: emailFieldVal, password: passwordFieldVal })
        onLoginSuccess()
      } else {
        await signup({ email: emailFieldVal, password: passwordFieldVal })
        if (isEmailVerificationRequired) {
          showEmailVerificationPending()
        } else {
          await login({ email: emailFieldVal, password: passwordFieldVal })
          onLoginSuccess()
        }
      }
      setEmailFieldVal('')
      setPasswordFieldVal('')
    } catch (err: unknown) {
      onError(err as Error)
    }
  }

  return {
    handleSubmit,
    emailFieldVal,
    passwordFieldVal,
    setEmailFieldVal,
    setPasswordFieldVal,
  }
}
