import { requestPasswordReset } from '../../../email/actions/passwordReset.js'
import { useState, useContext, FormEvent } from 'react'
import { Form, FormItemGroup, FormLabel, FormInput, SubmitButton } from '../Form'
import { AuthContext } from '../../Auth'

export const ForgotPasswordForm = () => {
  const { isLoading, setErrorMessage, setSuccessMessage, setIsLoading } = useContext(AuthContext)
  const [email, setEmail] = useState('')

  const onSubmit = async (event: FormEvent<HTMLFormElement>) => {
    event.preventDefault()
    setIsLoading(true)
    setErrorMessage(null)
    setSuccessMessage(null)
    try {
      await requestPasswordReset({ email })
      setSuccessMessage('Check your email for a password reset link.')
      setEmail('')
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
          <FormLabel>E-mail</FormLabel>
          <FormInput
            type="email"
            required
            value={email}
            onChange={(e) => setEmail(e.target.value)}
            disabled={isLoading}
          />
        </FormItemGroup>
        <FormItemGroup>
          <SubmitButton type="submit" disabled={isLoading}>
            Send password reset email
          </SubmitButton>
        </FormItemGroup>
      </Form>
    </>
  )
}
