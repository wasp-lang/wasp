import { useAuthContext } from '@wasp/lib-auth/browser'
import { useForm } from 'react-hook-form'

import { requestPasswordReset } from '../../../email/actions/passwordReset.js'
import { Form, FormItemGroup, FormLabel, FormInput, SubmitButton, FormError, emailFieldRules, emailInputProps, getAuthErrorMessage } from '@wasp/lib-sdk-core/browser'

type ForgotPasswordFormFields = { email: string };

// PRIVATE API
export const ForgotPasswordForm = () => {
  const { register, handleSubmit, reset, formState: { errors } } = useForm<ForgotPasswordFormFields>()
  const { isLoading, setErrorMessage, setSuccessMessage, setIsLoading } = useAuthContext()

  const onSubmit = async (data: ForgotPasswordFormFields): Promise<void> => {
    setIsLoading(true)
    setErrorMessage(null)
    setSuccessMessage(null)
    try {
      await requestPasswordReset(data)
      reset()
      setSuccessMessage('Check your email for a password reset link.')
    } catch (error: unknown) {
      setErrorMessage(getAuthErrorMessage(error));
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
            {...register('email', emailFieldRules)}
            {...emailInputProps}
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
