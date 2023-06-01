import { resetPassword } from '../../../email/actions/passwordReset.js'
import { useState, useContext, FormEvent } from 'react'
import { useLocation } from 'react-router-dom'
import { Form, FormItemGroup, FormLabel, FormInput, SubmitButton } from '../Form'
import { AuthContext } from '../../Auth'

export const ResetPasswordForm = () => {
  const { isLoading, setErrorMessage, setSuccessMessage, setIsLoading } = useContext(AuthContext)
  const location = useLocation()
  const token = new URLSearchParams(location.search).get('token')
  const [password, setPassword] = useState('')
  const [passwordConfirmation, setPasswordConfirmation] = useState('')

  const onSubmit = async (event: FormEvent<HTMLFormElement>) => {
    event.preventDefault()

    if (!token) {
      setErrorMessage({
        title:
          'The token is missing from the URL. Please check the link you received in your email.',
      })
      return
    }

    if (!password || password !== passwordConfirmation) {
      setErrorMessage({ title: `Passwords don't match!` })
      return
    }

    setIsLoading(true)
    setErrorMessage(null)
    setSuccessMessage(null)
    try {
      await resetPassword({ password, token })
      setSuccessMessage('Your password has been reset.')
      setPassword('')
      setPasswordConfirmation('')
    } catch (error) {
      setErrorMessage({
        title: error.message,
        description: error.data?.data?.message,
      })
    } finally {
      setIsLoading(false)
    }
  }

  return (
    <>
      <Form onSubmit={onSubmit}>
        <FormItemGroup>
          <FormLabel>New password</FormLabel>
          <FormInput
            type="password"
            required
            value={password}
            onChange={(e) => setPassword(e.target.value)}
            disabled={isLoading}
          />
        </FormItemGroup>
        <FormItemGroup>
          <FormLabel>Confirm new password</FormLabel>
          <FormInput
            type="password"
            required
            value={passwordConfirmation}
            onChange={(e) => setPasswordConfirmation(e.target.value)}
            disabled={isLoading}
          />
        </FormItemGroup>
        <FormItemGroup>
          <SubmitButton type="submit" disabled={isLoading}>
            Reset password
          </SubmitButton>
        </FormItemGroup>
      </Form>
    </>
  )
}
