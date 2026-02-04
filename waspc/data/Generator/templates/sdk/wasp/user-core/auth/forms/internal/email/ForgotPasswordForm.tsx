import { useForm } from 'react-hook-form'
import { useAuthContext } from '@wasp.sh/lib-auth/browser'

import { requestPasswordReset } from '../../../email/actions/passwordReset'
import { Form, FormItemGroup, FormLabel, FormInput, SubmitButton, FormError } from '../../../../../core/auth/forms/internal/Form'

// PRIVATE API
export const ForgotPasswordForm = () => {
  const { register, handleSubmit, reset, formState: { errors } } = useForm<{ email: string }>()
  const { isLoading, setErrorMessage, setSuccessMessage, setIsLoading } = useAuthContext()

  const onSubmit = async (data) => {
    setIsLoading(true)
    setErrorMessage(null)
    setSuccessMessage(null)
    try {
      await requestPasswordReset(data)
      reset()
      setSuccessMessage('Check your email for a password reset link.')
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
      <Form onSubmit={handleSubmit(onSubmit)}>
        <FormItemGroup>
          <FormLabel>E-mail</FormLabel>
          <FormInput
            {...register('email', {
              required: 'Email is required',
            })}
            type="email"
            disabled={isLoading}
          />
          {errors.email && <FormError>{errors.email.message}</FormError>}
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
