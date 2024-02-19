import { useContext } from 'react'
import { useForm } from 'react-hook-form'
import { resetPassword } from '../../../email/actions/passwordReset.js'
import { useLocation } from 'react-router-dom'
import { Form, FormItemGroup, FormLabel, FormInput, SubmitButton, FormError } from '../Form'
import { AuthContext } from '../../Auth'

// PRIVATE API
export const ResetPasswordForm = () => {
  const { register, handleSubmit, reset, formState: { errors } } = useForm<{ password: string; passwordConfirmation: string }>()
  const { isLoading, setErrorMessage, setSuccessMessage, setIsLoading } = useContext(AuthContext)
  const location = useLocation()
  const token = new URLSearchParams(location.search).get('token')
  const onSubmit = async (data) => {
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
          <FormLabel>New password</FormLabel>
          <FormInput
            {...register('password', {
              required: 'Password is required',
            })}
            type="password"
            disabled={isLoading}
          />
          {errors.passwordConfirmation && (
            <FormError>{errors.passwordConfirmation.message}</FormError>
          )}
        </FormItemGroup>
        <FormItemGroup>
          <FormLabel>Confirm new password</FormLabel>
          <FormInput
            {...register('passwordConfirmation', {
              required: 'Password confirmation is required',
            })}
            type="password"
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
