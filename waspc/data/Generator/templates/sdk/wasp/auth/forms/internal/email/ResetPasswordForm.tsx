import { useLocation } from 'react-router'
import { useResetPasswordForm } from '@wasp.sh/lib-auth/browser'

import { resetPassword } from '../../../email/actions/passwordReset.js'
import { Form, FormItemGroup, FormLabel, FormInput, SubmitButton, FormError } from '../Form'
import { MessageError, MessageSuccess } from '../Message'

// PRIVATE API
export const ResetPasswordForm = () => {
  const location = useLocation()
  const token = new URLSearchParams(location.search).get('token')
  const form = useResetPasswordForm({
    token,
    submit: ({ password, token }) => resetPassword({ password, token }),
  })

  return (
    <>
      {form.errorMessage && (
        <MessageError>
          {form.errorMessage.title}{form.errorMessage.description && ': '}{form.errorMessage.description}
        </MessageError>
      )}
      {form.successMessage && <MessageSuccess>{form.successMessage}</MessageSuccess>}
      <Form onSubmit={(event) => void form.submit(event)}>
        <FormItemGroup>
          <FormLabel>New password</FormLabel>
          <FormInput
            {...form.getFieldProps('password')}
            type="password"
          />
          {form.fieldErrors.password && (
            <FormError>{form.fieldErrors.password}</FormError>
          )}
        </FormItemGroup>
        <FormItemGroup>
          <FormLabel>Confirm new password</FormLabel>
          <FormInput
            {...form.getFieldProps('passwordConfirmation')}
            type="password"
          />
          {form.fieldErrors.passwordConfirmation && (
            <FormError>{form.fieldErrors.passwordConfirmation}</FormError>
          )}
        </FormItemGroup>
        <FormItemGroup>
          <SubmitButton type="submit" disabled={form.isSubmitting}>
            Reset password
          </SubmitButton>
        </FormItemGroup>
      </Form>
    </>
  )
}
