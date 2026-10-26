import { useAuthContext } from '@wasp.sh/lib-auth/browser'
import { useForm } from 'react-hook-form'
import { useLocation } from 'react-router'

import { resetPassword } from '../../../email/actions/passwordReset.js'
import { Form, FormItemGroup, FormLabel, FormPasswordInput, SubmitButton, FormError, getAuthErrorMessage } from '@wasp.sh/lib-sdk-core/browser'

type ResetPasswordFormFields = { password: string; passwordConfirmation: string };

// PRIVATE API
export const ResetPasswordForm = () => {
  const { register, handleSubmit, reset, formState: { errors } } = useForm<ResetPasswordFormFields>()
  const { isLoading, setErrorMessage, setSuccessMessage, setIsLoading } = useAuthContext()
  const location = useLocation()
  const token = new URLSearchParams(location.search).get('token')
  const onSubmit = async (data: ResetPasswordFormFields): Promise<void> => {
    if (!token) {
      setErrorMessage({
        title:
          'The token is missing from the URL. Please check the link you received in your email.',
      })
      return
    }

    if (!data.password || data.password !== data.passwordConfirmation) {
      setErrorMessage({ title: `Passwords don't match!` })
      return
    }

    setIsLoading(true)
    setErrorMessage(null)
    setSuccessMessage(null)
    try {
      await resetPassword({ password: data.password, token })
      reset()
      setSuccessMessage('Your password has been reset.')
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
          <FormLabel>New password</FormLabel>
          <FormPasswordInput
            {...register('password', {
              required: 'Password is required',
            })}
            disabled={isLoading}
          />
          {errors.passwordConfirmation && (
            <FormError>{errors.passwordConfirmation.message}</FormError>
          )}
        </FormItemGroup>
        <FormItemGroup>
          <FormLabel>Confirm new password</FormLabel>
          <FormPasswordInput
            {...register('passwordConfirmation', {
              required: 'Password confirmation is required',
            })}
            disabled={isLoading}
          />
          {errors.passwordConfirmation && (
            <FormError>{errors.passwordConfirmation.message}</FormError>
          )}
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
